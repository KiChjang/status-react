(ns status-im.hardwallet.core
  (:require [clojure.string :as string]
            [re-frame.core :as re-frame]
            [status-im.multiaccounts.create.core :as multiaccounts.create]
            [status-im.multiaccounts.logout.core :as multiaccounts.logout]
            [status-im.ethereum.core :as ethereum]
            [status-im.ui.components.colors :as colors]
            [status-im.ethereum.mnemonic :as mnemonic]
            [status-im.i18n :as i18n]
            [status-im.ui.screens.navigation :as navigation]
            [status-im.utils.config :as config]
            [status-im.utils.datetime :as utils.datetime]
            [status-im.utils.fx :as fx]
            [status-im.utils.platform :as platform]
            [status-im.utils.types :as types]
            [taoensso.timbre :as log]
            status-im.hardwallet.fx
            [status-im.ui.components.react :as react]
            [status-im.constants :as constants]
            [status-im.multiaccounts.update.core :as multiaccounts.update]
            [status-im.ui.components.bottom-sheet.core :as bottom-sheet]
            [status-im.multiaccounts.recover.core :as recover]
            [status-im.ethereum.eip55 :as eip55]
            [status-im.utils.keychain.core :as keychain]
            [status-im.hardwallet.nfc :as nfc]
            [status-im.native-module.core :as status]))

(def default-pin "000000")

(defn- vector->string [v]
  "Converts numbers stored in vector into string,
  e.g. [1 2 3 4 5 6] -> \"123456\""
  (apply str v))

(defn- find-multiaccount-by-keycard-instance-uid
  [db keycard-instance-uid]
  (when keycard-instance-uid
    (->> (:multiaccounts/multiaccounts db)
         vals
         (filter #(= keycard-instance-uid (:keycard-instance-uid %)))
         first)))

(defn- find-multiaccount-by-key-uid
  [db key-uid]
  (when key-uid
    (->> (:multiaccounts/multiaccounts db)
         vals
         (filter #(= (ethereum/normalized-hex key-uid) (:key-uid %)))
         first)))

(defn get-pairing
  ([db]
   (get-pairing db (get-in db [:hardwallet :application-info :key-uid])))
  ([db key-uid]
   (or
    (get-in db [:multiaccount :keycard-pairing])
    (get-in db [:hardwallet :secrets :pairing])
    (when key-uid
      (:keycard-pairing
       (find-multiaccount-by-key-uid db key-uid))))))

(re-frame/reg-fx
 :hardwallet/set-nfc-supported
 (fn [supported?]
   (nfc/set-nfc-supported? supported?)))

(fx/defn listen-to-hardware-back-button
  [{:keys [db]}]
  (when-not (get-in db [:hardwallet :back-button-listener])
    {:hardwallet/listen-to-hardware-back-button nil}))

(fx/defn remove-listener-to-hardware-back-button
  [{:keys [db]}]
  (when-let [listener (get-in db [:hardwallet :back-button-listener])]
    {:hardwallet/remove-listener-to-hardware-back-button listener}))

(fx/defn set-on-card-connected
  [{:keys [db]} on-connect]
  {:db (-> db
           (assoc-in [:hardwallet :on-card-connected] on-connect)
           (assoc-in [:hardwallet :last-on-card-connected] nil))})

(fx/defn stash-on-card-connected
  [{:keys [db]}]
  (let [on-connect (get-in db [:hardwallet :on-card-connected])]
    {:db (-> db
             (assoc-in [:hardwallet :last-on-card-connected] on-connect)
             (assoc-in [:hardwallet :on-card-connected] nil))}))

(fx/defn restore-on-card-connected
  [{:keys [db]}]
  (let [on-connect (or
                    (get-in db [:hardwallet :on-card-connected])
                    (get-in db [:hardwallet :last-on-card-connected]))]
    {:db (-> db
             (assoc-in [:hardwallet :on-card-connected] on-connect)
             (assoc-in [:hardwallet :last-on-card-connect] nil))}))

(fx/defn clear-on-card-connected
  [{:keys [db]}]
  {:db (-> db
           (assoc-in [:hardwallet :on-card-connected] nil)
           (assoc-in [:hardwallet :last-on-card-connected] nil))})

(fx/defn set-on-card-read
  [{:keys [db]} on-connect]
  {:db (-> db
           (assoc-in [:hardwallet :on-card-read] on-connect)
           (assoc-in [:hardwallet :last-on-card-read] nil))})

(fx/defn stash-on-card-read
  [{:keys [db]}]
  (let [on-connect (get-in db [:hardwallet :on-card-read])]
    {:db (-> db
             (assoc-in [:hardwallet :last-on-card-read] on-connect)
             (assoc-in [:hardwallet :on-card-read] nil))}))

(fx/defn restore-on-card-read
  [{:keys [db]}]
  (let [on-connect (or
                    (get-in db [:hardwallet :on-card-read])
                    (get-in db [:hardwallet :last-on-card-read]))]
    {:db (-> db
             (assoc-in [:hardwallet :on-card-read] on-connect)
             (assoc-in [:hardwallet :last-on-card-connect] nil))}))

(fx/defn clear-on-card-read
  [{:keys [db]}]
  {:db (-> db
           (assoc-in [:hardwallet :on-card-read] nil)
           (assoc-in [:hardwallet :last-on-card-read] nil))})

(fx/defn on-add-listener-to-hardware-back-button
  "Adds listener to hardware back button on Android.
  During keycard setup we show user a warning that setup will be cancelled
  when back button pressed. This prevents user from going back during setup
  flow as some of the actions changing keycard step could not be repeated."
  {:events [:hardwallet/add-listener-to-hardware-back-button]}
  [{:keys [db]} listener]
  {:db (assoc-in db [:hardwallet :back-button-listener] listener)})

(fx/defn hardwallet-connect-navigate-back-button-clicked
  [{:keys [db] :as cofx}]
  (fx/merge cofx
            (clear-on-card-connected)
            (navigation/navigate-back)))

(fx/defn enter-pin-navigate-back-button-clicked
  [{:keys [db] :as cofx}]
  (let [screen-before (set (take 4 (:navigation-stack db)))
        navigate-to-browser? (contains? screen-before :browser-stack)]
    (if navigate-to-browser?
      (fx/merge cofx
                (clear-on-card-connected)
                ;;TODO use new signing flow
                ;;(wallet/discard-transaction)
                (navigation/navigate-to-cofx :browser nil))
      (if (= :enter-pin-login (:view-id db))
        (navigation/navigate-to-cofx cofx :multiaccounts nil)
        (fx/merge cofx
                  (clear-on-card-connected)
                  (navigation/navigate-back))))))

(fx/defn remove-pairing-from-multiaccount
  [cofx {:keys [remove-instance-uid?]}]
  (fx/merge cofx
            (multiaccounts.update/multiaccount-update
             :keycard-pairing nil {})
            (multiaccounts.update/multiaccount-update
             :keycard-paired-on nil {})
            (when remove-instance-uid?
              (multiaccounts.update/multiaccount-update
               :keycard-instance-uid nil {}))))

(defn hardwallet-supported? []
  (and config/hardwallet-enabled?
       platform/android?
       (nfc/nfc-supported?)))

(fx/defn unauthorized-operation
  [{:keys [db] :as cofx}]
  (fx/merge cofx
            {:utils/show-popup {:title   ""
                                :content (i18n/label :t/keycard-unauthorized-operation)}}
            (clear-on-card-connected)
            (navigation/navigate-to-cofx :keycard-settings nil)))

(fx/defn show-no-keycard-applet-alert [_]
  {:utils/show-confirmation {:title               (i18n/label :t/no-keycard-applet-on-card)
                             :content             (i18n/label :t/keycard-applet-install-instructions)
                             :cancel-button-text  ""
                             :confirm-button-text :t/okay}})

(defn- card-state->setup-step [state]
  (case state
    :not-paired :pair
    :no-pairing-slots :no-slots
    :init :card-ready
    :multiaccount :import-multiaccount
    :begin))

(defn- get-card-state
  [{:keys [has-master-key?
           applet-installed?
           initialized?
           free-pairing-slots
           paired?]}]
  (cond

    (not applet-installed?)
    :blank

    (not initialized?)
    :pre-init

    (not has-master-key?)
    :init

    has-master-key?
    :multiaccount

    (and (not paired?)
         (zero? free-pairing-slots))
    :no-pairing-slots))

(fx/defn set-setup-step
  [{:keys [db]} card-state]
  {:db (assoc-in db [:hardwallet :setup-step] (card-state->setup-step card-state))})

(fx/defn show-keycard-has-multiaccount-alert
  [{:keys [db] :as cofx}]
  (fx/merge cofx
            {:db                      (assoc-in db [:hardwallet :setup-step] nil)
             :utils/show-confirmation {:title               nil
                                       :content             (i18n/label :t/keycard-has-multiaccount-on-it)
                                       :cancel-button-text  ""
                                       :confirm-button-text :t/okay}}))

(fx/defn dispatch-event
  [_ event]
  {:dispatch [event]})

(fx/defn load-preparing-screen
  {:events [:hardwallet/load-preparing-screen]}
  [{:keys [db] :as cofx}]
  (let [card-connected? (get-in db [:hardwallet :card-connected?])]
    (fx/merge cofx
              {:db (assoc-in db [:hardwallet :setup-step] :preparing)}
              (set-on-card-connected :hardwallet/load-preparing-screen)
              (when card-connected?
                (navigation/navigate-to-cofx :keycard-onboarding-preparing nil))
              (if card-connected?
                (dispatch-event :hardwallet/start-installation)
                (navigation/navigate-to-cofx :keycard-connection-lost-setup nil)))))

(fx/defn load-pin-screen
  [{:keys [db] :as cofx}]
  (fx/merge cofx
            {:db (-> db
                     (assoc-in [:hardwallet :setup-step] :pin)
                     (assoc-in [:hardwallet :pin] {:enter-step   :original
                                                   :original     []
                                                   :confirmation []}))}
            (navigation/navigate-to-cofx :keycard-onboarding-pin nil)))

(fx/defn load-pair-screen
  [{:keys [db] :as cofx}]
  (log/debug "[hardwallet] load-pair-screen")
  (fx/merge cofx
            {:db (-> db
                     (assoc-in [:hardwallet :setup-step] :pair))}
            (listen-to-hardware-back-button)
            (navigation/navigate-to-cofx :keycard-recovery-pair nil)))

(fx/defn load-pairing-screen
  {:events [:hardwallet/load-pairing-screen
            :keycard.onboarding.puk-code.ui/confirm-pressed]}
  [{:keys [db] :as cofx}]
  (let [card-connected? (get-in db [:hardwallet :card-connected?])]
    (fx/merge cofx
              {:db (assoc-in db [:hardwallet :setup-step] :pairing)}
              (set-on-card-connected :hardwallet/load-pairing-screen)
              (when card-connected?
                (navigation/navigate-to-cofx :keycard-pairing nil))
              (if card-connected?
                (dispatch-event :hardwallet/pair)
                (navigation/navigate-to-cofx :keycard-connection-lost-setup nil)))))

(fx/defn puk-code-next-pressed
  {:events [:keycard.onboarding.puk-code.ui/next-pressed]}
  [_]
  {:ui/show-confirmation {:title               (i18n/label :t/secret-keys-confirmation-title)
                          :content             (i18n/label :t/secret-keys-confirmation-text)
                          :confirm-button-text (i18n/label :t/yes)
                          :cancel-button-text  (i18n/label :t/cancel)
                          :on-accept           #(re-frame/dispatch [:keycard.onboarding.puk-code.ui/confirm-pressed])
                          :on-cancel           #()}})

(fx/defn cancel-setup-pressed
  {:events [:keycard.onboarding.ui/cancel-pressed
            :hardwallet/back-button-pressed
            :keycard.onboarding.recovery-phrase.ui/cancel-pressed
            :keycard.onboarding.connection-lost-setup.ui/cancel-setup-pressed]}
  [_]
  {:ui/show-confirmation {:title               (i18n/label :t/keycard-cancel-setup-title)
                          :content             (i18n/label :t/keycard-cancel-setup-text)
                          :confirm-button-text (i18n/label :t/yes)
                          :cancel-button-text  (i18n/label :t/no)
                          :on-accept           #(re-frame/dispatch [:keycard.onboarding.ui/cancel-confirm-pressed])
                          :on-cancel           #()}})

(fx/defn cancel-setup-confirm-pressed
  {:events [:keycard.onboarding.ui/cancel-confirm-pressed]}
  [{:keys [db] :as cofx}]
  (fx/merge cofx
            (remove-listener-to-hardware-back-button)
            (navigation/navigate-reset {:index   0
                                        :actions [{:routeName (if (seq (:multiaccounts/multiaccounts db))
                                                                :multiaccounts
                                                                :intro)}]})))

(fx/defn load-finishing-screen
  {:events [:keycard.onboarding.recovery-phrase-confirm-word2.ui/next-pressed
            :hardwallet/load-finishing-screen]}
  [{:keys [db] :as cofx}]
  (let [card-connected? (get-in db [:hardwallet :card-connected?])]
    (fx/merge cofx
              {:db (assoc-in db [:hardwallet :setup-step] :loading-keys)}
              (set-on-card-connected :hardwallet/load-finishing-screen)
              (when card-connected?
                (navigation/navigate-to-cofx :keycard-onboarding-finishing nil))
              (if card-connected?
                (dispatch-event :hardwallet/generate-and-load-key)
                (navigation/navigate-to-cofx :keycard-connection-lost-setup nil)))))

(fx/defn recovery-phrase-learn-more-pressed
  {:events [:keycard.onboarding.recovery-phrase.ui/learn-more-pressed]}
  [_]
  (.openURL react/linking "https://keycard.status.im"))

(fx/defn recovery-phrase-next-pressed
  {:events [:keycard.onboarding.recovery-phrase.ui/next-pressed]}
  [_]
  {:ui/show-confirmation {:title               (i18n/label :t/keycard-recovery-phrase-confirmation-title)
                          :content             (i18n/label :t/keycard-recovery-phrase-confirmation-text)
                          :confirm-button-text (i18n/label :t/yes)
                          :cancel-button-text  (i18n/label :t/cancel)
                          :on-accept           #(re-frame/dispatch [:keycard.onboarding.recovery-phrase.ui/confirm-pressed])
                          :on-cancel           #()}})

(fx/defn recovery-phrase-start-confirmation
  [{:keys [db] :as cofx}]
  (let [mnemonic (get-in db [:hardwallet :secrets :mnemonic])
        [word1 word2] (shuffle (map-indexed vector (clojure.string/split mnemonic #" ")))
        word1 (zipmap [:idx :word] word1)
        word2 (zipmap [:idx :word] word2)]
    (fx/merge cofx
              {:db (-> db
                       (assoc-in [:hardwallet :setup-step] :recovery-phrase-confirm-word1)
                       (assoc-in [:hardwallet :recovery-phrase :step] :word1)
                       (assoc-in [:hardwallet :recovery-phrase :confirm-error] nil)
                       (assoc-in [:hardwallet :recovery-phrase :input-word] nil)
                       (assoc-in [:hardwallet :recovery-phrase :word1] word1)
                       (assoc-in [:hardwallet :recovery-phrase :word2] word2))}
              (remove-listener-to-hardware-back-button))))

(fx/defn recovery-phrase-confirm-pressed
  {:events [:keycard.onboarding.recovery-phrase.ui/confirm-pressed]}
  [cofx]
  (fx/merge cofx
            (recovery-phrase-start-confirmation)
            (navigation/navigate-to-cofx :keycard-onboarding-recovery-phrase-confirm-word1 nil)))

(fx/defn recovery-phrase-next-word
  [{:keys [db]}]
  {:db (-> db
           (assoc-in [:hardwallet :recovery-phrase :step] :word2)
           (assoc-in [:hardwallet :recovery-phrase :confirm-error] nil)
           (assoc-in [:hardwallet :recovery-phrase :input-word] nil)
           (assoc-in [:hardwallet :setup-step] :recovery-phrase-confirm-word2))})

(fx/defn recovery-phrase-confirm-word-back-pressed
  {:events [:keycard.onboarding.recovery-phrase-confirm-word.ui/back-pressed]}
  [{:keys [db] :as cofx}]
  (if (= (:view-id db) :keycard-onboarding-recovery-phrase-confirm-word1)
    (navigation/navigate-to-cofx cofx :keycard-onboarding-recovery-phrase nil)
    (navigation/navigate-to-cofx cofx :keycard-onboarding-recovery-phrase-confirm-word1 nil)))

(fx/defn proceed-with-generating-key
  [{:keys [db] :as cofx}]
  (let [pin (or (get-in db [:hardwallet :secrets :pin])
                (vector->string (get-in db [:hardwallet :pin :current])))]
    (if (empty? pin)
      (fx/merge cofx
                {:db (assoc-in db [:hardwallet :pin] {:enter-step  :current
                                                      :on-verified :hardwallet/generate-and-load-key
                                                      :current     []})}
                (navigation/navigate-to-cofx :keycard-onboarding-pin nil))
      (load-finishing-screen cofx))))

(fx/defn recovery-phrase-confirm-word-next-pressed
  {:events [:keycard.onboarding.recovery-phrase-confirm-word.ui/next-pressed
            :keycard.onboarding.recovery-phrase-confirm-word.ui/input-submitted]}
  [{:keys [db] :as cofx}]
  (let [step (get-in db [:hardwallet :recovery-phrase :step])
        input-word (get-in db [:hardwallet :recovery-phrase :input-word])
        {:keys [word]} (get-in db [:hardwallet :recovery-phrase step])]
    (if (= word input-word)
      (if (= (:view-id db) :keycard-onboarding-recovery-phrase-confirm-word1)
        (fx/merge cofx
                  (recovery-phrase-next-word)
                  (navigation/navigate-to-cofx :keycard-onboarding-recovery-phrase-confirm-word2 nil))
        (proceed-with-generating-key cofx))
      {:db (assoc-in db [:hardwallet :recovery-phrase :confirm-error] (i18n/label :t/wrong-word))})))

(fx/defn recovery-phrase-confirm-word-input-changed
  {:events [:keycard.onboarding.recovery-phrase-confirm-word.ui/input-changed]}
  [{:keys [db]} input]
  {:db (assoc-in db [:hardwallet :recovery-phrase :input-word] input)})

(fx/defn pair-code-input-changed
  {:events [:keycard.onboarding.pair.ui/input-changed]}
  [{:keys [db]} input]
  {:db (assoc-in db [:hardwallet :secrets :password] input)})

(fx/defn load-recovery-pin-screen
  [{:keys [db] :as cofx}]
  (fx/merge cofx
            {:db (-> db
                     (assoc-in [:hardwallet :pin] {:enter-step          :import-multiaccount
                                                   :import-multiaccount []
                                                   :current             []}))}
            (listen-to-hardware-back-button)
            (navigation/navigate-to-cofx :keycard-recovery-pin nil)))

(fx/defn on-generate-mnemonic-success
  [{:keys [db] :as cofx} mnemonic]
  (fx/merge cofx
            {:db (-> db
                     (assoc-in [:hardwallet :setup-step] :recovery-phrase)
                     (assoc-in [:hardwallet :secrets :mnemonic] mnemonic))}
            (clear-on-card-connected)
            (navigation/navigate-to-cofx :keycard-onboarding-recovery-phrase nil)))

(fx/defn set-mnemonic
  [{:keys [db] :as cofx}]
  (let [selected-id (get-in db [:intro-wizard :selected-id])
        accounts    (get-in db [:intro-wizard :multiaccounts])
        mnemonic    (->> accounts
                         (filter (fn [{:keys [id]}]
                                   (= id selected-id)))
                         first
                         :mnemonic)]
    (fx/merge
     cofx
     {:db (assoc-in db [:hardwallet :secrets :mnemonic] mnemonic)}
     (on-generate-mnemonic-success mnemonic))))

(fx/defn generate-mnemonic
  [cofx]
  (let [{:keys [pairing]} (get-in cofx [:db :hardwallet :secrets])]
    {:hardwallet/generate-mnemonic {:pairing pairing
                                    :words   (string/join "\n" mnemonic/dictionary)}}))

(fx/defn proceed-with-generating-mnemonic
  [{:keys [db] :as cofx}]
  (let [pin (or (get-in db [:hardwallet :secrets :pin])
                (vector->string (get-in db [:hardwallet :pin :current])))]
    (if (empty? pin)
      (fx/merge cofx
                {:db (assoc-in db [:hardwallet :pin] {:enter-step  :current
                                                      :on-verified :hardwallet/generate-mnemonic
                                                      :current     []})}
                (navigation/navigate-to-cofx :keycard-onboarding-pin nil))
      (generate-mnemonic cofx))))

(fx/defn proceed-setup-with-initialized-card
  [{:keys [db] :as cofx} flow instance-uid]
  (log/debug "[hardwallet] proceed-setup-with-initialized-card"
             "instance-uid" instance-uid)
  (if (= flow :import)
    (navigation/navigate-to-cofx cofx :keycard-recovery-no-key nil)
    (let [pairing-data (get-in db [:hardwallet :pairings instance-uid])]
      (if pairing-data
        (fx/merge cofx
                  {:db (update-in db [:hardwallet :secrets] merge pairing-data)}
                  (listen-to-hardware-back-button)
                  (when (= flow :create)
                    (proceed-with-generating-mnemonic))
                  (when (= flow :recovery)
                    (proceed-with-generating-key)))
        (load-pair-screen cofx)))))

(fx/defn check-card-state
  {:events [:hardwallet/check-card-state]}
  [{:keys [db] :as cofx}]
  (let [app-info (get-in db [:hardwallet :application-info])
        flow (get-in db [:hardwallet :flow])
        {:keys [instance-uid key-uid]} app-info
        pairing (get-pairing db key-uid)
        app-info' (if pairing (assoc app-info :paired? true) app-info)
        card-state (get-card-state app-info')]
    (log/debug "[hardwallet] check-card-state"
               "card-state" card-state
               "flow" flow)
    (fx/merge cofx
              {:db (assoc-in db [:hardwallet :card-state] card-state)}
              (set-setup-step card-state)
              (when (and flow
                         (= card-state :init))
                (proceed-setup-with-initialized-card flow instance-uid))
              (when (= card-state :pre-init)
                (if (= flow :import)
                  (navigation/navigate-to-cofx :keycard-recovery-no-key nil)
                  (fn [cofx]
                    (fx/merge
                     cofx
                     (clear-on-card-read)
                     (load-pin-screen)))))
              (when (and (= card-state :multiaccount)
                         (= flow :import))
                (if (find-multiaccount-by-key-uid db key-uid)
                  (recover/show-existing-multiaccount-alert key-uid)
                  (if pairing
                    (load-recovery-pin-screen)
                    (load-pair-screen))))
              (when (= card-state :blank)
                (if (= flow :import)
                  (navigation/navigate-to-cofx :keycard-recovery-no-key nil)
                  (show-no-keycard-applet-alert)))
              (when (and (= card-state :multiaccount)
                         (#{:create :recovery} flow))
                (show-keycard-has-multiaccount-alert)))))

(fx/defn navigate-to-keycard-settings
  [{:keys [db] :as cofx}]
  (fx/merge cofx
            {:db (-> db
                     (assoc-in [:hardwallet :pin :on-verified] nil)
                     (assoc-in [:hardwallet :setup-step] nil))}
            (clear-on-card-connected)
            (navigation/navigate-to-cofx :keycard-settings nil)))

(fx/defn navigate-to-enter-pin-screen
  [{:keys [db] :as cofx}]
  (let [key-uid (get-in db [:hardwallet :application-info :key-uid])
        multiaccount-key-uid (get-in db [:multiaccount :key-uid])
        keycard-multiaccount? (boolean (get-in db [:multiaccount :keycard-pairing]))]
    (if (or (nil? keycard-multiaccount?)
            (and key-uid
                 (= key-uid multiaccount-key-uid)))
      (fx/merge cofx
                {:db (assoc-in db [:hardwallet :pin :current] [])}
                (navigation/navigate-to-cofx :enter-pin-settings nil))
      (unauthorized-operation cofx))))

(fx/defn keycard-storage-selected-for-recovery
  {:events [:recovery.ui/keycard-storage-selected]}
  [{:keys [db] :as cofx}]
  (fx/merge cofx
            {:db (assoc-in db [:hardwallet :flow] :recovery)}
            (navigation/navigate-to-cofx :keycard-recovery-enter-mnemonic nil)))

(fx/defn start-import-flow
  {:events [::recover-with-keycard-pressed]}
  [{:keys [db] :as cofx}]
  (fx/merge cofx
            {:db                           (assoc-in db [:hardwallet :flow] :import)
             :hardwallet/check-nfc-enabled nil}
            (bottom-sheet/hide-bottom-sheet)
            (navigation/navigate-to-cofx :keycard-recovery-intro nil)))

(fx/defn access-key-pressed
  {:events [:multiaccounts.recover.ui/recover-multiaccount-button-pressed]}
  [cofx]
  {:dispatch [:bottom-sheet/show-sheet :recover-sheet]})

(fx/defn recovery-keycard-selected
  {:events [:recovery.ui/keycard-option-pressed]}
  [{:keys [db] :as cofx}]
  (fx/merge cofx
            {:db                           (assoc-in db [:hardwallet :flow] :recovery)
             :hardwallet/check-nfc-enabled nil}
            (navigation/navigate-to-cofx :keycard-onboarding-intro nil)))

(fx/defn password-option-pressed
  [{:keys [db] :as cofx}]
  (if (= (get-in db [:hardwallet :flow]) :create)
    #())) ;;TODO with v1 flow

(defn settings-screen-did-load
  [{:keys [db] :as cofx}]
  (fx/merge
   cofx
   {:db (-> db
            (assoc-in [:hardwallet :pin :on-verified] nil)
            (assoc-in [:hardwallet :setup-step] nil))}
   (clear-on-card-connected)))

(defn reset-card-screen-did-load
  [{:keys [db]}]
  {:db (assoc-in db [:hardwallet :reset-card :disabled?] false)})

(defn enter-pin-screen-did-load
  [{:keys [db]}]
  (let [enter-step (get-in db [:hardwallet :pin :enter-step])]
    {:db (assoc-in db [:hardwallet :pin enter-step] [])}))

(defn hardwallet-connect-screen-did-load
  [{:keys [db]}]
  {:db (assoc-in db [:hardwallet :card-read-in-progress?] false)})

(defn multiaccounts-screen-did-load
  [{:keys [db]}]
  {:db (-> db
           (assoc-in [:hardwallet :setup-step] nil)
           (dissoc :multiaccounts/login))})

(defn authentication-method-screen-did-load
  [{:keys [db]}]
  {:db (assoc-in db [:hardwallet :setup-step] nil)})

(fx/defn on-register-card-events
  [{:keys [db]} listeners]
  {:db (update-in db [:hardwallet :listeners] merge listeners)})

(fx/defn get-keys-from-keycard
  [{:keys [db]}]
  (let [key-uid (get-in db [:multiaccounts/login :key-uid])
        pairing (get-in db [:multiaccounts/multiaccounts key-uid :keycard-pairing])
        pin (string/join (get-in db [:hardwallet :pin :login]))]
    (when (and pairing
               (seq pin))
      {:db                  (assoc-in db [:hardwallet :pin :status] :verifying)
       :hardwallet/get-keys {:pairing pairing
                             :pin     pin}})))

(fx/defn login-with-keycard
  {:events [:hardwallet/login-with-keycard]}
  [{:keys [db] :as cofx}]
  (let [application-info (get-in db [:hardwallet :application-info])
        key-uid (get-in db [:hardwallet :application-info :key-uid])
        multiaccount (get-in db [:multiaccounts/multiaccounts (get-in db [:multiaccounts/login :key-uid])])
        multiaccount-key-uid (get multiaccount :key-uid)
        multiaccount-mismatch? (or (nil? multiaccount)
                                   (not= multiaccount-key-uid key-uid))
        pairing (:keycard-pairing multiaccount)]
    (cond
      (empty? application-info)
      (navigation/navigate-to-cofx cofx :not-keycard nil)

      (empty? key-uid)
      (navigation/navigate-to-cofx cofx :keycard-blank nil)

      multiaccount-mismatch?
      (navigation/navigate-to-cofx cofx :keycard-wrong nil)

      (empty? pairing)
      (navigation/navigate-to-cofx cofx :keycard-unpaired nil)

      :else
      (get-keys-from-keycard cofx))))

(fx/defn show-wrong-keycard-alert
  [_ card-connected?]
  (when card-connected?
    {:utils/show-popup {:title   (i18n/label :t/wrong-card)
                        :content (i18n/label :t/wrong-card-text)}}))

(fx/defn on-get-application-info-success
  [{:keys [db] :as cofx} info on-success]
  (let [info' (-> info
                  (js->clj :keywordize-keys true)
                  (update :key-uid ethereum/normalized-hex))
        {:keys [pin-retry-counter puk-retry-counter]} info'
        view-id (:view-id db)
        connect-screen? (contains? #{:hardwallet-connect
                                     :hardwallet-connect-sign
                                     :hardwallet-connect-settings} view-id)
        {:keys [on-card-read]} (:hardwallet db)
        on-success' (or on-success on-card-read)
        enter-step (if (zero? pin-retry-counter)
                     :puk
                     (get-in db [:hardwallet :pin :enter-step]))]
    (log/debug "[hardwallet] on-get-application-info-success"
               "connect-screen?" connect-screen?
               "on-success" on-success')
    (fx/merge cofx
              {:db (-> db
                       (assoc-in [:hardwallet :pin :enter-step] enter-step)
                       (update-in [:hardwallet :pin :error-label] #(if (= :puk enter-step)
                                                                     :t/enter-puk-code-description
                                                                     %))
                       (assoc-in [:hardwallet :application-info] info')
                       (assoc-in [:hardwallet :application-info :applet-installed?] true)
                       (assoc-in [:hardwallet :application-info-error] nil))}
              (stash-on-card-read)
              (if (zero? puk-retry-counter)
                {:utils/show-popup {:title   (i18n/label :t/error)
                                    :content (i18n/label :t/keycard-blocked)}}
                (when on-success'
                  (dispatch-event on-success'))))))

(fx/defn on-get-application-info-error
  [{:keys [db] :as cofx} error]
  (log/debug "[hardwallet] application info error " error)
  (let [on-card-read (get-in db [:hardwallet :on-card-read])
        on-card-connected (get-in db [:hardwallet :on-card-connected])
        connect-screen? (= (:view-id db) :hardwallet-connect)
        login? (= on-card-read :hardwallet/login-with-keycard)
        tag-was-lost? (= "Tag was lost." (:error error))]
    (if tag-was-lost?
      (navigation/navigate-to-cofx cofx :keycard-connection-lost nil)
      (if login?
        (fx/merge cofx
                  (clear-on-card-read)
                  (navigation/navigate-to-cofx :not-keycard nil))
        (fx/merge cofx
                  {:db (assoc-in db [:hardwallet :application-info-error] error)}
                  (when (= on-card-connected :hardwallet/prepare-to-sign)
                    (show-wrong-keycard-alert true))
                  (when-not connect-screen?
                    (clear-on-card-read))
                  (when on-card-read
                    (dispatch-event on-card-read)))))))

(fx/defn set-nfc-supported
  [_ supported?]
  {:hardwallet/set-nfc-supported supported?})

(fx/defn keycard-option-pressed
  {:events [:onboarding.ui/keycard-option-pressed]}
  [{:keys [db] :as cofx}]
  (let [flow (get-in db [:hardwallet :flow])]
    (fx/merge cofx
              {:hardwallet/check-nfc-enabled nil}
              (if (= flow :import)
                (navigation/navigate-to-cofx :keycard-recovery-intro nil)
                (navigation/navigate-to-cofx :keycard-onboarding-intro nil)))))

(fx/defn keycard-connection-lost-cancel-pressed
  {:events [:keycard.connection-lost.ui/cancel-pressed]}
  [{:keys [db] :as cofx}]
  (fx/merge
   cofx
   (clear-on-card-connected)
   (clear-on-card-read)
   (if (contains? (set (take 3 (:navigation-stack db)))
                  :keycard-login-pin)
     (navigation/navigate-to-cofx :multiaccounts nil)
     (navigation/navigate-back))))

(fx/defn start-onboarding-flow
  {:events [:keycard.recovery.no-key.ui/generate-key-pressed
            :keycard/start-onboarding-flow]}
  [{:keys [db] :as cofx}]
  (fx/merge cofx
            {:db                           (assoc-in db [:hardwallet :flow] :create)
             :hardwallet/check-nfc-enabled nil}
            (navigation/navigate-to-cofx :keycard-onboarding-intro nil)))

(fx/defn begin-setup-pressed
  {:events [:keycard.onboarding.intro.ui/begin-setup-pressed
            :keycard.recovery.intro.ui/begin-recovery-pressed]}
  [{:keys [db] :as cofx}]
  (let [nfc-enabled? (get-in db [:hardwallet :nfc-enabled?])
        flow (get-in db [:hardwallet :flow])]
    (fx/merge cofx
              {:db (-> db
                       (update :hardwallet
                               dissoc :secrets :card-state :multiaccount-wallet-address
                               :multiaccount-whisper-public-key
                               :application-info)
                       (assoc-in [:hardwallet :setup-step] :begin)
                       (assoc-in [:hardwallet :pin :on-verified] nil))}
              (set-on-card-connected :hardwallet/get-application-info)
              (set-on-card-read :hardwallet/check-card-state)
              (if nfc-enabled?
                (if (= flow :import)
                  (navigation/navigate-to-cofx :keycard-recovery-start nil)
                  (navigation/navigate-to-cofx :keycard-onboarding-start nil))
                (navigation/navigate-to-cofx :keycard-nfc-on nil)))))

(fx/defn proceed-to-login
  [{:keys [db] :as cofx}]
  (let [{:keys [card-connected? nfc-enabled?]} (:hardwallet db)]
    (if nfc-enabled?
      (fx/merge cofx
                (set-on-card-connected :hardwallet/get-application-info)
                (set-on-card-read :hardwallet/login-with-keycard)
                (if card-connected?
                  (login-with-keycard)
                  (navigation/navigate-to-cofx :keycard-login-connect-card nil)))
      (navigation/navigate-to-cofx cofx :keycard-nfc-on nil))))

(fx/defn open-nfc-settings-pressed
  {:events [:keycard.onboarding.nfc-on/open-nfc-settings-pressed]}
  [_]
  {:hardwallet/open-nfc-settings nil})

(fx/defn on-check-nfc-enabled-success
  {:events [:hardwallet.callback/check-nfc-enabled-success]}
  [{:keys [db] :as cofx} nfc-enabled?]
  (let [flow (get-in db [:hardwallet :flow])
        login? (= :login (get-in db [:hardwallet :pin :enter-step]))]
    (fx/merge cofx
              {:db (assoc-in db [:hardwallet :nfc-enabled?] nfc-enabled?)}
              (when (and nfc-enabled?
                         (= (:view-id db)
                            :keycard-nfc-on))
                (if flow
                  (if (= flow :import)
                    (navigation/navigate-to-cofx :keycard-recovery-start nil)
                    (navigation/navigate-to-cofx :keycard-onboarding-start nil))
                  (when login?
                    (proceed-to-login)))))))

(fx/defn success-button-pressed [cofx]
  (navigation/navigate-to-cofx cofx :home nil))

(fx/defn recovery-success-finish-pressed
  {:events [:keycard.recovery.success/finish-pressed]}
  [{:keys [db] :as cofx}]
  (fx/merge cofx
            {:db (update db :hardwallet dissoc
                         :multiaccount-wallet-address
                         :multiaccount-whisper-public-key)}
            (navigation/navigate-to-cofx :welcome nil)))

(fx/defn login-got-it-pressed
  {:events [:keycard.login.ui/got-it-pressed
            :keycard.login.ui/dismiss-pressed
            :keycard.login.pin.ui/cancel-pressed]}
  [{:keys [db] :as cofx}]
  (fx/merge cofx
            {:db db}
            (navigation/navigate-to-cofx :multiaccounts nil)))

(fx/defn login-pin-more-icon-pressed
  {:events [:keycard.login.pin.ui/more-icon-pressed]}
  [{:keys [db] :as cofx}]
  (bottom-sheet/show-bottom-sheet cofx {:view :keycard.login/more}))

(fx/defn login-create-key-pressed
  {:events [:keycard.login.ui/create-new-key-pressed]}
  [{:keys [db] :as cofx}]
  (fx/merge cofx
            (bottom-sheet/hide-bottom-sheet)
            (start-onboarding-flow)))

(fx/defn login-add-key-pressed
  {:events [:keycard.login.ui/add-key-pressed]}
  [{:keys [db] :as cofx}]
  (start-import-flow cofx))

(fx/defn login-remember-me-changed
  {:events [:keycard.login.ui/remember-me-changed]}
  [{:keys [db] :as cofx} value]
  (fx/merge cofx
            {:db (assoc-in db [:hardwallet :remember-me?] value)}))

(fx/defn login-pair-card-pressed
  {:events [:keycard.login.ui/pair-card-pressed]}
  [{:keys [db] :as cofx}]
  (log/debug "[hardwallet] load-pair-card-pressed")
  (fx/merge cofx
            {:db (assoc-in db [:hardwallet :flow] :login)}
            (navigation/navigate-to-cofx :keycard-recovery-pair nil)))

(fx/defn change-pin-pressed
  [{:keys [db] :as cofx}]
  (let [pin-retry-counter (get-in db [:hardwallet :application-info :pin-retry-counter])
        enter-step (if (zero? pin-retry-counter) :puk :current)]
    (fx/merge cofx
              {:db
               (assoc-in db [:hardwallet :pin] {:enter-step   enter-step
                                                :current      []
                                                :puk          []
                                                :original     []
                                                :confirmation []
                                                :status       nil
                                                :error-label  nil
                                                :on-verified  :hardwallet/proceed-to-change-pin})}
              (navigate-to-enter-pin-screen))))

(fx/defn proceed-to-change-pin
  [{:keys [db] :as cofx}]
  (fx/merge cofx
            {:db (-> db
                     (assoc-in [:hardwallet :pin :enter-step] :original)
                     (assoc-in [:hardwallet :pin :status] nil))}
            (navigation/navigate-to-cofx :enter-pin-settings nil)))

(fx/defn unpair-card-pressed
  [_]
  {:ui/show-confirmation {:title               (i18n/label :t/unpair-card)
                          :content             (i18n/label :t/unpair-card-confirmation)
                          :confirm-button-text (i18n/label :t/yes)
                          :cancel-button-text  (i18n/label :t/no)
                          :on-accept           #(re-frame/dispatch [:keycard-settings.ui/unpair-card-confirmed])
                          :on-cancel           #()}})

(fx/defn unpair-card-confirmed
  [{:keys [db] :as cofx}]
  (let [pin-retry-counter (get-in db [:hardwallet :application-info :pin-retry-counter])
        enter-step (if (zero? pin-retry-counter) :puk :current)]
    (fx/merge cofx
              {:db (assoc-in db [:hardwallet :pin] {:enter-step  enter-step
                                                    :current     []
                                                    :puk         []
                                                    :status      nil
                                                    :error-label nil
                                                    :on-verified :hardwallet/unpair})}
              (navigate-to-enter-pin-screen))))

(fx/defn unpair
  [{:keys [db]}]
  (let [pin (vector->string (get-in db [:hardwallet :pin :current]))
        pairing (get-pairing db)]
    {:hardwallet/unpair {:pin     pin
                         :pairing pairing}}))

(fx/defn unpair-and-delete
  [{:keys [db]}]
  (let [pin (vector->string (get-in db [:hardwallet :pin :current]))
        pairing (get-pairing db)]
    {:hardwallet/unpair-and-delete {:pin     pin
                                    :pairing pairing}}))

(fx/defn remove-key-with-unpair
  [{:keys [db] :as cofx}]
  (let [pin (vector->string (get-in db [:hardwallet :pin :current]))
        pairing (get-pairing db)
        card-connected? (get-in db [:hardwallet :card-connected?])]
    (if card-connected?
      {:hardwallet/remove-key-with-unpair {:pin     pin
                                           :pairing pairing}}
      (fx/merge cofx
                (set-on-card-connected :hardwallet/remove-key-with-unpair)
                (navigation/navigate-to-cofx :keycard-connection-lost nil)))))

(fx/defn on-remove-key-success
  [{:keys [db] :as cofx}]
  (let [key-uid (get-in db [:multiaccount :key-uid])
        instance-uid (get-in db [:hardwallet :application-info :instance-uid])
        pairings (get-in db [:hardwallet :pairings])]
    (fx/merge cofx
              {:db                 (-> db
                                       (update :multiaccounts/multiaccounts dissoc key-uid)
                                       (assoc-in [:hardwallet :secrets] nil)
                                       (update-in [:hardwallet :pairings] dissoc (keyword instance-uid))
                                       (assoc-in [:hardwallet :whisper-public-key] nil)
                                       (assoc-in [:hardwallet :wallet-address] nil)
                                       (assoc-in [:hardwallet :application-info] nil)
                                       (assoc-in [:hardwallet :pin] {:status      nil
                                                                     :error-label nil
                                                                     :on-verified nil}))
               :hardwallet/persist-pairings (dissoc pairings (keyword instance-uid))
               ;;FIXME delete multiaccount
               :utils/show-popup   {:title   ""
                                    :content (i18n/label :t/card-reseted)}}
              (clear-on-card-connected)
              (multiaccounts.logout/logout))))

(fx/defn on-remove-key-error
  [{:keys [db] :as cofx} error]
  (log/debug "[hardwallet] remove key error" error)
  (let [tag-was-lost? (= "Tag was lost." (:error error))]
    (fx/merge cofx
              (if tag-was-lost?
                (fx/merge cofx
                          {:db               (assoc-in db [:hardwallet :pin :status] nil)
                           :utils/show-popup {:title   (i18n/label :t/error)
                                              :content (i18n/label :t/cannot-read-card)}}
                          (set-on-card-connected :hardwallet/remove-key-with-unpair)
                          (navigation/navigate-to-cofx :keycard-connection-lost nil))
                (show-wrong-keycard-alert true)))))

(fx/defn on-export-key-success
  {:events [:hardwallet.callback/on-export-key-success]}
  [{:keys [db] :as cofx} pubkey]
  (let [multiaccount-address (get-in db [:multiaccount :address])
        instance-uid (get-in db [:hardwallet :application-info :instance-uid])
        callback-fn (get-in db [:hardwallet :on-export-success])
        pairings (get-in db [:hardwallet :pairings])
        event-to-dispatch (callback-fn pubkey)]
    (re-frame/dispatch event-to-dispatch)
    (fx/merge cofx
              (clear-on-card-connected))))

(def pin-mismatch-error #"Unexpected error SW, 0x63C\d+")

(fx/defn get-application-info
  [{:keys [db]} pairing on-card-read]
  (let [key-uid (get-in db [:hardwallet :application-info :key-uid])
        pairing' (or pairing
                     (when key-uid
                       (get-pairing db key-uid)))]
    {:hardwallet/get-application-info {:pairing    pairing'
                                       :on-success on-card-read}}))

(fx/defn on-export-key-error
  {:events [:hardwallet.callback/on-export-key-error]}
  [{:keys [db] :as cofx} error]
  (log/debug "[hardwallet] export key error" error)
  (let [tag-was-lost? (= "Tag was lost." (:error error))]
    (cond tag-was-lost?
          (fx/merge cofx
                    {:db               (assoc-in db [:hardwallet :pin :status] nil)
                     :utils/show-popup {:title   (i18n/label :t/error)
                                        :content (i18n/label :t/cannot-read-card)}}
                    (set-on-card-connected :wallet.accounts/generate-new-keycard-account)
                    (navigation/navigate-to-cofx :keycard-connection-lost nil))
          (re-matches pin-mismatch-error (:error error))
          (fx/merge cofx
                    {:db (update-in db [:hardwallet :pin] merge {:status       :error
                                                                 :enter-step   :export-key
                                                                 :puk          []
                                                                 :current      []
                                                                 :original     []
                                                                 :confirmation []
                                                                 :sign         []
                                                                 :error-label  :t/pin-mismatch})}
                    (navigation/navigate-back)
                    (get-application-info (get-pairing db) nil))
          :else (show-wrong-keycard-alert cofx true))))

(fx/defn on-delete-success
  [{:keys [db] :as cofx}]
  (let [key-uid (get-in db [:multiaccount :key-uid])]
    (fx/merge cofx
              {:db                 (-> db
                                       (update :multiaccounts/multiaccounts dissoc key-uid)
                                       (assoc-in [:hardwallet :secrets] nil)
                                       (assoc-in [:hardwallet :application-info] nil)
                                       (assoc-in [:hardwallet :pin] {:status      nil
                                                                     :error-label nil
                                                                     :on-verified nil}))
               ;;FIXME delete multiaccount
               :utils/show-popup   {:title   ""
                                    :content (i18n/label :t/card-reseted)}}
              (clear-on-card-connected)
              (multiaccounts.logout/logout))))

(fx/defn on-delete-error
  [{:keys [db] :as cofx} error]
  (log/debug "[hardwallet] delete error" error)
  (fx/merge cofx
            {:db                              (assoc-in db [:hardwallet :pin] {:status      nil
                                                                               :error-label nil
                                                                               :on-verified nil})
             :hardwallet/get-application-info nil
             :utils/show-popup                {:title   ""
                                               :content (i18n/label :t/something-went-wrong)}}
            (clear-on-card-connected)
            (navigation/navigate-to-cofx :keycard-settings nil)))

(fx/defn reset-card-pressed
  [cofx]
  (navigation/navigate-to-cofx cofx :reset-card nil))

(fx/defn delete-card
  [{:keys [db] :as cofx}]
  (let [key-uid (get-in db [:hardwallet :application-info :key-uid])
        multiaccount-key-uid (get-in db [:multiaccount :key-uid])]
    (if (and key-uid
             (= key-uid multiaccount-key-uid))
      {:hardwallet/delete nil}
      (unauthorized-operation cofx))))

(fx/defn navigate-to-reset-card-screen
  [cofx]
  (navigation/navigate-to-cofx cofx :reset-card nil))

(fx/defn reset-card-next-button-pressed
  [{:keys [db]}]
  {:db       (assoc-in db [:hardwallet :reset-card :disabled?] true)
   :dispatch [:hardwallet/proceed-to-reset-card]})

(fx/defn proceed-to-reset-card
  [{:keys [db] :as cofx}]
  (let [pin-retry-counter (get-in db [:hardwallet :application-info :pin-retry-counter])
        enter-step (if (zero? pin-retry-counter) :puk :current)]
    (fx/merge cofx
              {:db (assoc-in db [:hardwallet :pin] {:enter-step  enter-step
                                                    :current     []
                                                    :puk         []
                                                    :status      nil
                                                    :error-label nil
                                                    :on-verified :hardwallet/remove-key-with-unpair})}
              (set-on-card-connected :hardwallet/navigate-to-enter-pin-screen)
              (navigate-to-enter-pin-screen))))

(fx/defn error-button-pressed
  [{:keys [db] :as cofx}]
  (let [card-connected? (get-in db [:hardwallet :card-connected?])
        on-card-connected (get-in db [:hardwallet :on-card-connected])]
    (if card-connected?
      {:dispatch [on-card-connected]}
      (navigation/navigate-to-cofx cofx :hardwallet-connect nil))))

(fx/defn pair* [_ password]
  {:hardwallet/pair {:password password}})

(fx/defn pair
  [{:keys [db] :as cofx}]
  (let [{:keys [password]} (get-in cofx [:db :hardwallet :secrets])
        card-connected? (get-in db [:hardwallet :card-connected?])]
    (fx/merge cofx
              (set-on-card-connected :hardwallet/pair)
              (when card-connected?
                (pair* password))
              (if card-connected?
                (navigation/navigate-to-cofx :keycard-pairing nil)
                (navigation/navigate-to-cofx :keycard-connection-lost-setup nil)))))

(fx/defn pair-code-next-button-pressed
  {:events [:keycard.onboarding.pair.ui/input-submitted
            :keycard.onboarding.pair.ui/next-pressed]}
  [{:keys [db] :as cofx}]
  (let [pairing (get-in db [:hardwallet :secrets :pairing])
        paired-on (get-in db [:hardwallet :secrets :paired-on] (utils.datetime/timestamp))]
    (fx/merge cofx
              (if pairing
                {:db (-> db
                         (assoc-in [:hardwallet :setup-step] :import-multiaccount)
                         (assoc-in [:hardwallet :secrets :paired-on] paired-on))}
                (pair)))))

(fx/defn return-back-from-nfc-settings [{:keys [db]}]
  (when (or (get-in db [:hardwallet :flow])
            (get-in db [:hardwallet :pin :enter-step])
            (get-in db [:hardwallet :on-card-connected])
            (get-in db [:hardwallet :on-card-read]))
    {:hardwallet/check-nfc-enabled nil}))

(defn- proceed-to-pin-confirmation [fx]
  (assoc-in fx [:db :hardwallet :pin :enter-step] :confirmation))

(fx/defn change-pin
  [{:keys [db] :as cofx}]
  (let [pairing (get-pairing db)
        new-pin (vector->string (get-in db [:hardwallet :pin :original]))
        current-pin (vector->string (get-in db [:hardwallet :pin :current]))
        setup-step (get-in db [:hardwallet :setup-step])
        card-connected? (get-in db [:hardwallet :card-connected?])]
    (if (= setup-step :pin)
      (load-preparing-screen cofx)
      (if card-connected?
        (fx/merge cofx
                  {:db                    (assoc-in db [:hardwallet :pin :status] :verifying)
                   :hardwallet/change-pin {:new-pin     new-pin
                                           :current-pin current-pin
                                           :pairing     pairing}})
        (fx/merge cofx
                  (set-on-card-connected :hardwallet/change-pin)
                  (navigation/navigate-to-cofx :hardwallet-connect nil))))))

(fx/defn dispatch-on-verified-event
  [{:keys [db]} event]
  {:dispatch [event]
   :db       (assoc-in db [:hardwallet :pin :on-verified] nil)})

(fx/defn on-unblock-pin-success
  [{:keys [db] :as cofx}]
  (let [pairing (get-pairing db)]
    (fx/merge cofx
              {:hardwallet/get-application-info {:pairing pairing}
               :db (-> db
                       (update-in [:hardwallet :pin] merge {:status       nil
                                                            :enter-step   :original
                                                            :current      [0 0 0 0 0 0]
                                                            :confirmation []
                                                            :puk          []
                                                            :error-label  nil}))}
              (navigation/navigate-to-cofx :enter-pin-settings nil))))

(defn on-unblock-pin-error
  [{:keys [db]} error]
  (let [pairing (get-pairing db)]
    (log/debug "[hardwallet] unblock pin error" error)
    {:hardwallet/get-application-info {:pairing pairing}
     :db (update-in db [:hardwallet :pin] merge {:status      :error
                                                 :error-label :t/puk-mismatch
                                                 :enter-step  :puk
                                                 :puk         []})}))

(defn- tag-lost-exception? [code error]
  (or
   (= code "android.nfc.TagLostException")
   (= error "Tag was lost.")))

(fx/defn on-verify-pin-success
  [{:keys [db] :as cofx}]
  (let [on-verified (get-in db [:hardwallet :pin :on-verified])
        pairing (get-pairing db)]
    (fx/merge cofx
              {:db (update-in db [:hardwallet :pin] merge {:status      nil
                                                           :error-label nil})}
              (clear-on-card-connected)
              (when-not (contains? #{:hardwallet/unpair
                                     :hardwallet/generate-and-load-key
                                     :hardwallet/remove-key-with-unpair
                                     :hardwallet/unpair-and-delete} on-verified)
                (get-application-info pairing nil))
              (when on-verified
                (dispatch-on-verified-event on-verified)))))

(defn on-verify-pin-error
  [{:keys [db] :as cofx} error]
  (let [tag-was-lost? (= "Tag was lost." (:error error))
        setup? (boolean (get-in db [:hardwallet :setup-step]))
        exporting? (get-in db [:hardwallet :on-export-success])]
    (log/debug "[hardwallet] verify pin error" error)
    (cond tag-was-lost?
          (fx/merge cofx
                    {:db               (assoc-in db [:hardwallet :pin :status] nil)
                     :utils/show-popup {:title   (i18n/label :t/error)
                                        :content (i18n/label :t/cannot-read-card)}}
                    (set-on-card-connected :hardwallet/verify-pin)
                    (navigation/navigate-to-cofx (if setup?
                                                   :keycard-connection-lost-setup
                                                   :keycard-connection-lost) nil))
          (re-matches pin-mismatch-error (:error error))
          (fx/merge cofx
                    {:db (update-in db [:hardwallet :pin] merge {:status       :error
                                                                 :enter-step   :current
                                                                 :puk          []
                                                                 :current      []
                                                                 :original     []
                                                                 :confirmation []
                                                                 :sign         []
                                                                 :error-label  :t/pin-mismatch})}
                    (when-not setup?
                      (if exporting?
                        (navigation/navigate-back)
                        (navigation/navigate-to-cofx :enter-pin-settings nil)))
                    (get-application-info (get-pairing db) nil))
          :else (show-wrong-keycard-alert true))))

(fx/defn on-change-pin-success
  [{:keys [db] :as cofx}]
  (let [pin (get-in db [:hardwallet :pin :original])]
    (fx/merge cofx
              {:db               (assoc-in db [:hardwallet :pin] {:status       nil
                                                                  :login        pin
                                                                  :confirmation []
                                                                  :error-label  nil})
               :utils/show-popup {:title   ""
                                  :content (i18n/label :t/pin-changed)}}
              (clear-on-card-connected)
              (when (:multiaccounts/login db)
                (navigation/navigate-to-cofx :keycard-login-pin nil))
              (when (:multiaccounts/login db)
                (get-keys-from-keycard))
              (when (:multiaccount/multiaccount db)
                (multiaccounts.logout/logout)))))

(fx/defn on-change-pin-error
  [{:keys [db] :as cofx} error]
  (log/debug "[hardwallet] change pin error" error)
  (let [tag-was-lost? (= "Tag was lost." (:error error))]
    (fx/merge cofx
              (if tag-was-lost?
                (fx/merge cofx
                          {:db               (assoc-in db [:hardwallet :pin :status] nil)
                           :utils/show-popup {:title   (i18n/label :t/error)
                                              :content (i18n/label :t/cannot-read-card)}}
                          (set-on-card-connected :hardwallet/change-pin)
                          (navigation/navigate-to-cofx :hardwallet-connect nil))
                (if (re-matches pin-mismatch-error (:error error))
                  (fx/merge cofx
                            {:db (update-in db [:hardwallet :pin] merge {:status       :error
                                                                         :enter-step   :current
                                                                         :puk          []
                                                                         :current      []
                                                                         :original     []
                                                                         :confirmation []
                                                                         :sign         []
                                                                         :error-label  :t/pin-mismatch})}
                            (navigation/navigate-to-cofx :enter-pin-settings nil)
                            (get-application-info (get-pairing db) nil))
                  (show-wrong-keycard-alert true))))))

(fx/defn on-unpair-success
  [{:keys [db] :as cofx}]
  (let [instance-uid (get-in db [:hardwallet :application-info :instance-uid])
        pairings (get-in db [:hardwallet :pairings])]
    (fx/merge cofx
              {:db                          (-> db
                                                (assoc-in [:hardwallet :secrets] nil)
                                                (update-in [:hardwallet :pairings] dissoc (keyword instance-uid))
                                                (assoc-in [:hardwallet :pin] {:status      nil
                                                                              :error-label nil
                                                                              :on-verified nil}))
               :hardwallet/persist-pairings (dissoc pairings (keyword instance-uid))
               :utils/show-popup            {:title   ""
                                             :content (i18n/label :t/card-unpaired)}}
              (clear-on-card-connected)
              (remove-pairing-from-multiaccount nil)
              (navigation/navigate-to-cofx :keycard-settings nil))))

(fx/defn on-unpair-error
  [{:keys [db] :as cofx} error]
  (log/debug "[hardwallet] unpair error" error)
  (fx/merge cofx
            {:db                              (assoc-in db [:hardwallet :pin] {:status      nil
                                                                               :error-label nil
                                                                               :on-verified nil})
             :hardwallet/get-application-info nil
             :utils/show-popup                {:title   ""
                                               :content (i18n/label :t/something-went-wrong)}}
            (clear-on-card-connected)
            (navigation/navigate-to-cofx :keycard-settings nil)))

(defn verify-pin
  [{:keys [db] :as cofx}]
  (let [pin (vector->string (get-in db [:hardwallet :pin :current]))
        pairing (get-pairing db)
        card-connected? (get-in db [:hardwallet :card-connected?])
        setup? (boolean (get-in db [:hardwallet :setup-step]))]
    (if card-connected?
      (fx/merge cofx
                {:db                    (assoc-in db [:hardwallet :pin :status] :verifying)
                 :hardwallet/verify-pin {:pin     pin
                                         :pairing pairing}})
      (fx/merge cofx
                (set-on-card-connected :hardwallet/verify-pin)
                (navigation/navigate-to-cofx (if setup?
                                               :keycard-connection-lost-setup
                                               :keycard-processing) nil)))))

(defn unblock-pin
  [{:keys [db] :as cofx}]
  (let [puk (vector->string (get-in db [:hardwallet :pin :puk]))
        key-uid (get-in db [:hardwallet :application-info :key-uid])
        card-connected? (get-in db [:hardwallet :card-connected?])
        pairing (get-pairing db key-uid)]
    (if card-connected?
      {:db                     (assoc-in db [:hardwallet :pin :status] :verifying)
       :hardwallet/unblock-pin {:puk     puk
                                :new-pin default-pin
                                :pairing pairing}}
      (fx/merge cofx
                (set-on-card-connected :hardwallet/unblock-pin)
                (navigation/navigate-to-cofx :keycard-connection-lost nil)))))

(def pin-code-length 6)
(def puk-code-length 12)

(fx/defn handle-pin-input
  [{:keys [db]} enter-step]
  (let [numbers-entered (count (get-in db [:hardwallet :pin enter-step]))]
    (when (or (= numbers-entered pin-code-length)
              (= numbers-entered puk-code-length))
      {:dispatch [:hardwallet/process-pin-input]})))

(fx/defn update-pin
  [{:keys [db] :as cofx} number enter-step]
  (let [numbers-entered (count (get-in db [:hardwallet :pin enter-step]))
        need-update? (if (= enter-step :puk)
                       (< numbers-entered puk-code-length)
                       (< numbers-entered pin-code-length))]
    (fx/merge cofx
              {:db (cond-> (assoc-in db [:hardwallet :pin :status] nil)
                     need-update? (update-in [:hardwallet :pin enter-step] (fnil conj []) number))}
              (when need-update?
                (handle-pin-input enter-step)))))

(defn- pin-enter-error [fx error-label]
  (update-in fx [:db :hardwallet :pin] merge {:status       :error
                                              :error-label  error-label
                                              :enter-step   :original
                                              :original     []
                                              :confirmation []}))

(fx/defn navigate-to-connect-screen
  [{:keys [db] :as cofx} screen-name]
  (let [modal? (get-in db [:navigation/screen-params :wallet-send-modal-stack :modal?])]
    (navigation/navigate-to-cofx cofx
                                 (if modal?
                                   :hardwallet-connect-modal
                                   screen-name)
                                 nil)))

(fx/defn sign
  {:events [:hardwallet/sign]}
  [{:keys [db] :as cofx}]
  (let [card-connected? (get-in db [:hardwallet :card-connected?])
        pairing (get-pairing db)
        multiaccount-keycard-instance-uid (get-in db [:multiaccount :keycard-instance-uid])
        instance-uid (get-in db [:hardwallet :application-info :instance-uid])
        keycard-match? (= multiaccount-keycard-instance-uid instance-uid)
        hash (get-in db [:hardwallet :hash])
        pin (vector->string (get-in db [:hardwallet :pin :sign]))]
    (if (and card-connected?
             keycard-match?)
      {:db              (-> db
                            (assoc-in [:hardwallet :card-read-in-progress?] true)
                            (assoc-in [:hardwallet :pin :status] :verifying))
       :hardwallet/sign {:hash    (ethereum/naked-address hash)
                         :pairing pairing
                         :pin     pin}}
      (fx/merge cofx
                {:db (assoc-in db [:signing/sign :keycard-step] :signing)}
                (set-on-card-connected :hardwallet/sign)
                (when-not keycard-match?
                  (show-wrong-keycard-alert card-connected?))))))

(fx/defn sign-typed-data
  {:events [:hardwallet/sign-typed-data]}
  [{:keys [db] :as cofx}]
  (let [card-connected? (get-in db [:hardwallet :card-connected?])
        hash (get-in db [:hardwallet :hash])
        _ (log/info ":hardwallet/sign-typed-data" hash "card-connected?" card-connected?)]
    (if card-connected?
      {:db                      (-> db
                                    (assoc-in [:hardwallet :card-read-in-progress?] true)
                                    (assoc-in [:signing/sign :keycard-step] :signing))
       :hardwallet/sign-typed-data {:hash (ethereum/naked-address hash)}}
      (fx/merge cofx
                (set-on-card-connected :hardwallet/sign-typed-data)
                {:db (assoc-in db [:signing/sign :keycard-step] :signing)}
                #_(navigation/navigate-to-cofx :hardwallet-connect nil)))))

(fx/defn store-hash-and-sign-typed
  {:events [:hardwallet/store-hash-and-sign-typed]}
  [{:keys [db] :as cofx} result]
  (log/info "# IN store-hash-and-sign-typed")
  (let [{:keys [result error]} (types/json->clj result)]
    (fx/merge cofx
              {:db (assoc-in db [:hardwallet :hash] result)}
              sign-typed-data)))

(fx/defn prepare-to-sign
  {:events [:hardwallet/prepare-to-sign]}
  [{:keys [db] :as cofx}]
  (let [card-connected? (get-in db [:hardwallet :card-connected?])
        pairing (get-pairing db)]
    (if card-connected?
      (fx/merge cofx
                {:db (assoc-in db [:signing/sign :keycard-step] :signing)}
                (get-application-info pairing :hardwallet/sign))
      (fx/merge cofx
                {:db (assoc-in db [:signing/sign :keycard-step] :connect)}
                (set-on-card-connected :hardwallet/prepare-to-sign)))))

(fx/defn import-multiaccount
  {:events [:hardwallet/import-multiaccount]}
  [{:keys [db] :as cofx}]
  (let [{:keys [pairing]} (get-in db [:hardwallet :secrets])
        instance-uid (get-in db [:hardwallet :application-info :instance-uid])
        key-uid (get-in db [:hardwallet :application-info :key-uid])
        pairing' (or pairing (get-pairing db key-uid))
        pin (vector->string (get-in db [:hardwallet :pin :import-multiaccount]))]
    (fx/merge cofx
              {:db                  (-> db
                                        (assoc-in [:hardwallet :multiaccount :instance-uid] instance-uid)
                                        (assoc-in [:hardwallet :secrets] {:pairing   pairing'
                                                                          :paired-on (utils.datetime/timestamp)}))
               :hardwallet/get-keys {:pairing    pairing'
                                     :pin        pin
                                     :on-success :hardwallet.callback/on-generate-and-load-key-success}})))

(fx/defn load-recovering-key-screen
  {:events [:hardwallet/load-recovering-key-screen]}
  [{:keys [db] :as cofx}]
  (let [card-connected? (get-in db [:hardwallet :card-connected?])]
    (fx/merge cofx
              (set-on-card-connected :hardwallet/load-recovering-key-screen)
              (when card-connected?
                (dispatch-event :hardwallet/import-multiaccount))
              (if card-connected?
                (navigation/navigate-to-cofx :keycard-recovery-recovering nil)
                (navigation/navigate-to-cofx :keycard-connection-lost-setup nil)))))

(fx/defn generate-new-keycard-account
  {:events [:wallet.accounts/generate-new-keycard-account]}
  [{:keys [db] :as cofx}]
  (let [path-num (inc (get-in db [:multiaccount :latest-derived-path]))
        path (str constants/path-wallet-root "/" path-num)
        card-connected? (get-in db [:hardwallet :card-connected?])
        pin (vector->string (get-in db [:hardwallet :pin :export-key]))
        pairing (get-pairing db)]
    (if card-connected?
      (fx/merge cofx
                {:db (assoc-in db [:hardwallet :on-export-success]
                               #(vector :wallet.accounts/account-generated
                                        {:name (str "Account " path-num)
                                         ;; Strip leading 04 prefix denoting uncompressed key format
                                         :address (eip55/address->checksum (str "0x" (ethereum/public-key->address (subs % 2))))
                                         :public-key (str "0x" %)
                                         :path path
                                         :color (rand-nth colors/account-colors)}))
                 :hardwallet/export-key {:pin pin :pairing pairing :path path}}
                (navigation/navigate-to-cofx :keycard-processing nil)
                (set-on-card-connected :wallet.accounts/generate-new-keycard-account))
      (fx/merge cofx
                (set-on-card-connected :wallet.accounts/generate-new-keycard-account)
                (navigation/navigate-to-cofx :keycard-processing nil)))))

; PIN enter steps:
; login - PIN is used to login
; sign - PIN for transaction sign
; current - current PIN to perform actions which require PIN auth
; original - new PIN when user changes it or creates new one
; confirmation - confirmation for new PIN
(fx/defn process-pin-input
  [{:keys [db]}]
  (let [enter-step (get-in db [:hardwallet :pin :enter-step])
        pin (get-in db [:hardwallet :pin enter-step])
        numbers-entered (count pin)]
    (log/debug "[hardwallet] process-pin-input"
               "enter-step" enter-step)
    (cond-> {:db (assoc-in db [:hardwallet :pin :status] nil)}

      (and (= enter-step :login)
           (= 6 numbers-entered))
      (proceed-to-login)

      (and (= enter-step :original)
           (= pin-code-length numbers-entered))
      (proceed-to-pin-confirmation)

      (and (= enter-step :original)
           (= pin-code-length numbers-entered)
           (= default-pin (vector->string pin)))
      (pin-enter-error :t/cannot-use-default-pin)

      (and (= enter-step :import-multiaccount)
           (= pin-code-length numbers-entered))
      (load-recovering-key-screen)

      (and (= enter-step :current)
           (= pin-code-length numbers-entered))
      (verify-pin)

      (and (= enter-step :export-key)
           (= pin-code-length numbers-entered))
      (generate-new-keycard-account)

      (and (= enter-step :sign)
           (= pin-code-length numbers-entered))
      (prepare-to-sign)

      (and (= enter-step :puk)
           (= puk-code-length numbers-entered))
      (unblock-pin)

      (and (= enter-step :confirmation)
           (= (get-in db [:hardwallet :pin :original])
              (get-in db [:hardwallet :pin :confirmation])))
      (change-pin)

      (and (= enter-step :confirmation)
           (= pin-code-length numbers-entered)
           (not= (get-in db [:hardwallet :pin :original])
                 (get-in db [:hardwallet :pin :confirmation])))
      (pin-enter-error :t/pin-mismatch))))

(fx/defn load-loading-keys-screen
  [{:keys [db] :as cofx}]
  (let [card-connected? (get-in db [:hardwallet :card-connected?])]
    (fx/merge cofx
              {:db (assoc-in db [:hardwallet :setup-step] :loading-keys)}
              (set-on-card-connected :hardwallet/load-loading-keys-screen)
              (if card-connected?
                (dispatch-event :hardwallet/generate-and-load-key)
                (navigation/navigate-to-cofx :hardwallet-connect nil)))))

(fx/defn load-generating-mnemonic-screen
  [{:keys [db] :as cofx}]
  (let [card-connected? (get-in db [:hardwallet :card-connected?])]
    (fx/merge cofx
              {:db (assoc-in db [:hardwallet :setup-step] :generating-mnemonic)}
              (set-on-card-connected :hardwallet/load-generating-mnemonic-screen)
              (if card-connected?
                (dispatch-event :hardwallet/generate-mnemonic)
                (navigation/navigate-to-cofx :hardwallet-connect nil)))))

(fx/defn on-card-connected
  [{:keys [db] :as cofx} _]
  (log/debug "[hardwallet] card connected")
  (let [instance-uid (get-in db [:hardwallet :application-info :instance-uid])
        key-uid (get-in db [:hardwallet :application-info :key-uid])
        accounts-screen? (= :multiaccounts (:view-id db))
        should-read-instance-uid? (nil? instance-uid)
        on-card-connected (get-in db [:hardwallet :on-card-connected])
        on-card-read (cond
                       should-read-instance-uid? :hardwallet/get-application-info
                       :else (get-in db [:hardwallet :on-card-read]))
        pairing (get-pairing db key-uid)]
    (log/debug "[hardwallet] on-card-connected"
               "on-card-connected" on-card-connected
               "on-card-read" on-card-read)
    (fx/merge cofx
              {:db (-> db
                       (assoc-in [:hardwallet :card-connected?] true)
                       (assoc-in [:hardwallet :card-read-in-progress?] (boolean on-card-read)))}
              (when on-card-connected
                (dispatch-event on-card-connected))
              (stash-on-card-connected)
              (when (and on-card-read
                         (nil? on-card-connected))
                (get-application-info pairing on-card-read)))))

(fx/defn on-card-disconnected
  [{:keys [db] :as cofx} _]
  (log/debug "[hardwallet] card disconnected ")
  (let [setup-running? (get-in db [:hardwallet :setup-step])
        on-card-connected (get-in db [:hardwallet :on-card-connected])]
    (fx/merge cofx
              {:db (-> db
                       (assoc-in [:hardwallet :card-connected?] false)
                       (assoc-in [:hardwallet :card-read-in-progress?] false))}
              (restore-on-card-connected)
              (restore-on-card-read)
              (when (and setup-running?
                         on-card-connected)
                (navigation/navigate-to-cofx :keycard-connection-lost-setup nil)))))

(fx/defn begin-setup-button-pressed
  [{:keys [db]}]
  {:db (-> db
           (assoc-in [:hardwallet :setup-step] :pin)
           (assoc-in [:hardwallet :pin :enter-step] :original)
           (assoc-in [:hardwallet :pin :original] [])
           (assoc-in [:hardwallet :pin :confirmation] []))})

(fx/defn start-installation
  [{:keys [db] :as cofx}]
  (let [card-state (get-in db [:hardwallet :card-state])
        pin (vector->string (get-in db [:hardwallet :pin :original]))]
    (case card-state

      :pre-init
      {:hardwallet/init-card pin}

      (do
        (log/debug (str "Cannot start keycard installation from state: " card-state))
        (fx/merge cofx
                  {:utils/show-popup {:title   (i18n/label :t/error)
                                      :content (i18n/label :t/something-went-wrong)}}
                  (navigation/navigate-to-cofx :hardwallet-authentication-method nil))))))

(fx/defn on-install-applet-and-init-card-success
  {:events [:hardwallet.callback/on-install-applet-and-init-card-success]}
  [{:keys [db] :as cofx} secrets]
  (let [secrets' (js->clj secrets :keywordize-keys true)]
    (fx/merge cofx
              {:hardwallet/get-application-info nil
               :db                              (-> db
                                                    (assoc-in [:hardwallet :card-state] :init)
                                                    (assoc-in [:hardwallet :setup-step] :secret-keys)
                                                    (update-in [:hardwallet :secrets] merge secrets'))}
              (clear-on-card-connected)
              (listen-to-hardware-back-button)
              (navigation/navigate-to-cofx :keycard-onboarding-puk-code nil))))

(def on-init-card-success on-install-applet-and-init-card-success)

(fx/defn process-error [{:keys [db] :as cofx} code error]
  (if (tag-lost-exception? code error)
    (navigation/navigate-to-cofx cofx :keycard-connection-lost nil)
    {:db (assoc-in db [:hardwallet :setup-step] :error)}))

(fx/defn on-install-applet-and-init-card-error
  [{:keys [db] :as cofx} {:keys [code error]}]
  (log/debug "[hardwallet] install applet and init card error: " error)
  (fx/merge cofx
            {:db (assoc-in db [:hardwallet :setup-error] error)}
            (set-on-card-connected :hardwallet/load-preparing-screen)
            (process-error code error)))

(def on-init-card-error on-install-applet-and-init-card-error)

(fx/defn set-multiaccount-pairing
  [{:keys [db] :as cofx} {:keys [address] :as multiaccount} pairing paired-on]
  (fx/merge cofx
            (multiaccounts.update/multiaccount-update
             :keycard-pairing pairing {})
            (multiaccounts.update/multiaccount-update
             :keycard-paired-on paired-on {})))

(fx/defn on-retrieve-pairings-success
  {:events [:hardwallet.callback/on-retrieve-pairings-success]}
  [{:keys [db]} pairings]
  {:db (assoc-in db [:hardwallet :pairings] pairings)})

(fx/defn on-pair-success
  "When pairing to device has completed, we need to persist pairing data to
  local storage. That's needed to ensure that during keycard setup
  keycard won't run out of pairings slots, ie. we don't pair the same
  card to the same device more than one time. Also, this allows the user to proceed
  with setup and skip the pairing step if the pairing was already done during a previous
  unfinished setup."
  [{:keys [db] :as cofx} pairing]
  (let [setup-step (get-in db [:hardwallet :setup-step])
        flow (get-in db [:hardwallet :flow])
        instance-uid (get-in db [:hardwallet :application-info :instance-uid])
        multiaccount (find-multiaccount-by-keycard-instance-uid db instance-uid)
        paired-on (utils.datetime/timestamp)
        pairings (assoc (get-in db [:hardwallet :pairings]) instance-uid {:pairing   pairing
                                                                          :paired-on paired-on})
        next-step (if (= setup-step :pair)
                    :begin
                    :card-ready)]
    (fx/merge cofx
              {:hardwallet/persist-pairings pairings
               :db                          (-> db
                                                (assoc-in [:hardwallet :pairings] pairings)
                                                (assoc-in [:hardwallet :application-info :paired?] true)
                                                (assoc-in [:hardwallet :setup-step] next-step)
                                                (assoc-in [:hardwallet :secrets :pairing] pairing)
                                                (assoc-in [:hardwallet :secrets :paired-on] paired-on))}
              (clear-on-card-connected)
              (when multiaccount
                (set-multiaccount-pairing multiaccount pairing paired-on))
              (when (= flow :login)
                (navigation/navigate-to-cofx :multiaccounts nil))
              (when (= flow :recovery)
                (proceed-with-generating-key))
              (when (= flow :import)
                (load-recovery-pin-screen))
              (when (= flow :create)
                (set-mnemonic)))))

(fx/defn on-pair-error
  [{:keys [db] :as cofx} {:keys [error code]}]
  (log/debug "[hardwallet] pair error: " error)
  (let [setup-step (get-in db [:hardwallet :setup-step])
        flow (get-in db [:hardwallet :flow])]
    (log/debug "[hardwallet] on-pair-error")
    (fx/merge cofx
              {:db (assoc-in db [:hardwallet :setup-error] (i18n/label :t/invalid-pairing-password))}
              (set-on-card-connected (if (= setup-step :pairing)
                                       :hardwallet/load-pairing-screen
                                       :hardwallet/pair))
              (when (= flow :import)
                (navigation/navigate-to-cofx :keycard-recovery-pair nil))
              (when (not= setup-step :enter-pair-code)
                (process-error code error)))))

(fx/defn on-generate-mnemonic-error
  [{:keys [db] :as cofx} {:keys [error code]}]
  (log/debug "[hardwallet] generate mnemonic error: " error)
  (fx/merge cofx
            {:db (assoc-in db [:hardwallet :setup-error] error)}
            (set-on-card-connected :hardwallet/load-generating-mnemonic-screen)
            (process-error code error)))

(defn- show-recover-confirmation []
  {:ui/show-confirmation {:title               (i18n/label :t/are-you-sure?)
                          :content             (i18n/label :t/are-you-sure-description)
                          :confirm-button-text (clojure.string/upper-case (i18n/label :t/yes))
                          :cancel-button-text  (i18n/label :t/see-it-again)
                          :on-accept           #(re-frame/dispatch [:hardwallet.ui/recovery-phrase-confirm-pressed])
                          :on-cancel           #(re-frame/dispatch [:hardwallet.ui/recovery-phrase-cancel-pressed])}})

(fx/defn recovery-phrase-confirm-word
  [{:keys [db]}]
  (let [step (get-in db [:hardwallet :recovery-phrase :step])
        input-word (get-in db [:hardwallet :recovery-phrase :input-word])
        {:keys [word]} (get-in db [:hardwallet :recovery-phrase step])]
    (if (= word input-word)
      (if (= step :word1)
        (recovery-phrase-next-word db)
        (show-recover-confirmation))
      {:db (assoc-in db [:hardwallet :recovery-phrase :confirm-error] (i18n/label :t/wrong-word))})))

(fx/defn card-ready-next-button-pressed
  [{:keys [db] :as cofx}]
  (let [pin (get-in db [:hardwallet :secrets :pin])
        pin-already-set? (boolean pin)]
    (if pin-already-set?
      (if (= (get-in db [:hardwallet :flow]) :create)
        (load-generating-mnemonic-screen cofx)
        {:db (assoc-in db [:hardwallet :setup-step] :recovery-phrase)})
      (fx/merge cofx
                {:db (-> db
                         (assoc-in [:hardwallet :setup-step] :pin)
                         (assoc-in [:hardwallet :pin :enter-step] :current)
                         (assoc-in [:hardwallet :pin :on-verified] :hardwallet/proceed-to-generate-mnemonic)
                         (assoc-in [:hardwallet :pin :current] [])
                         (assoc-in [:hardwallet :pin :original] nil))}))))

(fx/defn proceed-to-generate-mnemonic
  [{:keys [db] :as cofx}]
  (if (= (get-in db [:hardwallet :flow]) :create)
    (load-generating-mnemonic-screen cofx)
    {:db (assoc-in db [:hardwallet :setup-step] :recovery-phrase)}))

(fx/defn recovery-phrase-next-button-pressed
  [{:keys [db] :as cofx}]
  (if (= (get-in db [:hardwallet :flow]) :create)
    (recovery-phrase-start-confirmation cofx)
    (let [mnemonic (get-in db [:multiaccounts/recover :passphrase])]
      (fx/merge cofx
                {:db (assoc-in db [:hardwallet :secrets :mnemonic] mnemonic)}
                (load-loading-keys-screen)))))

(fx/defn import-multiaccount-back-button-pressed
  [cofx]
  (navigation/navigate-to-cofx cofx :hardwallet-authentication-method nil))

(fx/defn import-multiaccount-next-button-pressed
  [{:keys [db] :as cofx}]
  (fx/merge cofx
            {:db (-> db
                     (assoc-in [:hardwallet :pin :enter-step] :import-multiaccount))}
            (navigation/navigate-to-cofx :enter-pin-login nil)))

(fx/defn generate-and-load-key
  {:events [:hardwallet/generate-and-load-key]}
  [{:keys [db] :as cofx}]
  (let [{:keys [mnemonic pairing pin]} (get-in db [:hardwallet :secrets])
        {:keys [selected-id multiaccounts]} (:intro-wizard db)
        user-selected-mnemonic (->> multiaccounts
                                    (filter #(= (:id %) selected-id))
                                    first
                                    :mnemonic)
        recovery-mnemonic (get-in db [:intro-wizard :passphrase])
        mnemonic' (or user-selected-mnemonic mnemonic recovery-mnemonic)
        pin' (or pin (vector->string (get-in db [:hardwallet :pin :current])))]
    (fx/merge cofx
              {:hardwallet/generate-and-load-key {:mnemonic mnemonic'
                                                  :pairing  pairing
                                                  :pin      pin'}}
              (navigation/navigate-to-cofx :keycard-onboarding-finishing nil))))

(re-frame/reg-fx
 ::generate-name-and-photo
 (fn [public-key]
   (status/gfycat-identicon-async
    public-key
    (fn [whisper-name photo-path]
      (re-frame/dispatch
       [::on-name-and-photo-generated whisper-name photo-path])))))

(fx/defn create-keycard-multiaccount
  [{:keys [db] :as cofx}]
  (let [{{:keys [multiaccount secrets flow]} :hardwallet} db
        {:keys [address
                name
                photo-path
                public-key
                whisper-public-key
                wallet-public-key
                wallet-root-public-key
                whisper-address
                wallet-address
                wallet-root-address
                whisper-private-key
                encryption-public-key
                instance-uid
                key-uid]} multiaccount
        {:keys [pairing paired-on]} secrets
        {:keys [name photo-path]}
        (if (nil? name)
          ;; name might have been generated during recovery via passphrase
          (get-in db [:intro-wizard :derived constants/path-whisper-keyword])
          {:name       name
           :photo-path photo-path})]
    ;; if a name is still `nil` we have to generate it before multiaccount's
    ;; creation otherwise spec validation will fail
    (if (nil? name)
      {::generate-name-and-photo whisper-public-key}
      (fx/merge cofx
                {:db (-> db
                         (assoc-in [:hardwallet :setup-step] nil)
                         (assoc :intro-wizard nil))}
                (multiaccounts.create/on-multiaccount-created
                 {:derived              {constants/path-wallet-root-keyword
                                         {:publicKey wallet-root-public-key
                                          :address (eip55/address->checksum wallet-root-address)}
                                         constants/path-whisper-keyword
                                         {:publicKey  whisper-public-key
                                          :address    (eip55/address->checksum whisper-address)
                                          :name       name
                                          :photo-path photo-path}
                                         constants/path-default-wallet-keyword
                                         {:publicKey wallet-public-key
                                          :address   (eip55/address->checksum wallet-address)}}
                  :address              address
                  :public-key           public-key
                  :keycard-instance-uid instance-uid
                  :keyUid               (ethereum/normalized-hex key-uid)
                  :keycard-pairing      pairing
                  :keycard-paired-on    paired-on
                  :chat-key             whisper-private-key}
                 encryption-public-key
                 {})
                (if (= flow :import)
                  (navigation/navigate-to-cofx :keycard-recovery-success nil)
                  (navigation/navigate-to-cofx :welcome nil))))))

(fx/defn on-name-and-photo-generated
  {:events [::on-name-and-photo-generated]
   :interceptors [(re-frame/inject-cofx :random-guid-generator)
                  (re-frame/inject-cofx ::multiaccounts.create/get-signing-phrase)]}
  [{:keys [db] :as cofx} whisper-name photo-path]
  (fx/merge
   cofx
   {:db (update-in db [:hardwallet :multiaccount]
                   (fn [multiacc]
                     (assoc multiacc
                            :name whisper-name
                            :photo-path photo-path)))}
   (create-keycard-multiaccount)))

(fx/defn on-generate-and-load-key-success
  [{:keys [db random-guid-generator] :as cofx} data]
  (let [account-data (js->clj data :keywordize-keys true)]
    (fx/merge cofx
              {:db (-> db
                       (assoc-in [:hardwallet :multiaccount]
                                 (-> account-data
                                     (update :address ethereum/normalized-hex)
                                     (update :whisper-address ethereum/normalized-hex)
                                     (update :wallet-address ethereum/normalized-hex)
                                     (update :wallet-root-address ethereum/normalized-hex)
                                     (update :public-key ethereum/normalized-hex)
                                     (update :whisper-public-key ethereum/normalized-hex)
                                     (update :wallet-public-key ethereum/normalized-hex)
                                     (update :wallet-root-public-key ethereum/normalized-hex)
                                     (update :instance-uid #(get-in db [:hardwallet :multiaccount :instance-uid] %))))
                       (assoc-in [:hardwallet :multiaccount-wallet-address] (:wallet-address account-data))
                       (assoc-in [:hardwallet :multiaccount-whisper-public-key] (:whisper-public-key account-data))
                       (assoc-in [:hardwallet :application-info :key-uid] (:key-uid account-data))
                       (update :hardwallet dissoc :recovery-phrase)
                       (update-in [:hardwallet :secrets] dissoc :pin :puk :password)
                       (assoc :multiaccounts/new-installation-id (random-guid-generator))
                       (update-in [:hardwallet :secrets] dissoc :mnemonic))}
              (clear-on-card-connected)
              (remove-listener-to-hardware-back-button)
              (create-keycard-multiaccount))))

(fx/defn on-generate-and-load-key-error
  [{:keys [db] :as cofx} {:keys [error code]}]
  (log/debug "[hardwallet] generate and load key error: " error)
  (fx/merge cofx
            {:db (assoc-in db [:hardwallet :setup-error] error)}
            (set-on-card-connected :hardwallet/load-loading-keys-screen)
            (process-error code error)))

(fx/defn on-login-success
  {:events [:keycard.login.callback/login-success]}
  [cofx result]
  (log/debug "loginWithKeycard success: " result))

(fx/defn on-get-keys-success
  [{:keys [db] :as cofx} data]
  (let [{:keys [key-uid encryption-public-key whisper-private-key] :as account-data} (js->clj data :keywordize-keys true)
        {:keys [photo-path name]} (get-in db [:multiaccounts/multiaccounts key-uid])
        key-uid (get-in db [:hardwallet :application-info :key-uid])
        multiaccount-data (types/clj->json {:name       name
                                            :key-uid    key-uid
                                            :photo-path photo-path})
        save-keys? (get-in db [:multiaccounts/login :save-password?])]
    (fx/merge
     cofx
     {:db
      (-> db
          (assoc-in [:hardwallet :pin :status] nil)
          (assoc-in [:hardwallet :pin :login] [])
          (assoc-in [:hardwallet :multiaccount]
                    (update account-data :whisper-public-key ethereum/normalized-hex))
          (assoc-in [:hardwallet :flow] nil)
          (update :multiaccounts/login assoc
                  :password encryption-public-key
                  :key-uid key-uid
                  :photo-path photo-path
                  :name name))

      :hardwallet/get-application-info {:pairing (get-pairing db key-uid)}
      :hardwallet/login-with-keycard   {:multiaccount-data multiaccount-data
                                        :password          encryption-public-key
                                        :chat-key          whisper-private-key}}
     (when save-keys?
       (keychain/save-hardwallet-keys key-uid encryption-public-key whisper-private-key))
     (clear-on-card-connected)
     (clear-on-card-read))))

(fx/defn on-hardwallet-keychain-keys
  {:events [:multiaccounts.login.callback/get-hardwallet-keys-success]}
  [{:keys [db] :as cofx} key-uid [encryption-public-key whisper-private-key :as creds]]
  (if (nil? creds)
    (navigation/navigate-to-cofx cofx :keycard-login-pin nil)
    (let [{:keys [photo-path name]} (get-in db [:multiaccounts/multiaccounts key-uid])
          multiaccount-data         (types/clj->json {:name       name
                                                      :key-uid    key-uid
                                                      :photo-path photo-path})
          account-data {:key-uid               key-uid
                        :encryption-public-key encryption-public-key
                        :whisper-private-key   whisper-private-key}]
      {:db
       (-> db
           (assoc-in [:hardwallet :pin :status] nil)
           (assoc-in [:hardwallet :pin :login] [])
           (assoc-in [:hardwallet :multiaccount]
                     (update account-data :whisper-public-key ethereum/normalized-hex))
           (assoc-in [:hardwallet :flow] nil)
           (update :multiaccounts/login assoc
                   :password encryption-public-key
                   :key-uid key-uid
                   :photo-path photo-path
                   :name name
                   :save-password? true))
       :hardwallet/login-with-keycard
       {:multiaccount-data multiaccount-data
        :password          encryption-public-key
        :chat-key          whisper-private-key}})))

(fx/defn on-get-keys-error
  [{:keys [db] :as cofx} error]
  (log/debug "[hardwallet] get keys error: " error)
  (let [tag-was-lost? (= "Tag was lost." (:error error))
        key-uid (get-in db [:hardwallet :application-info :key-uid])
        flow (get-in db [:hardwallet :flow])]
    (if tag-was-lost?
      (fx/merge cofx
                {:db (assoc-in db [:hardwallet :pin :status] nil)}
                (navigation/navigate-to-cofx :keycard-connection-lost nil))
      (if (re-matches pin-mismatch-error (:error error))
        (fx/merge cofx
                  {:hardwallet/get-application-info {:pairing (get-pairing db key-uid)}
                   :db                              (update-in db [:hardwallet :pin] merge {:status              :error
                                                                                            :login               []
                                                                                            :import-multiaccount []
                                                                                            :error-label         :t/pin-mismatch})}
                  (if (= flow :import)
                    (navigation/navigate-to-cofx :keycard-recovery-pin nil)
                    (navigation/navigate-to-cofx :keycard-login-pin nil)))
        (show-wrong-keycard-alert true)))))

(fx/defn send-transaction-with-signature
  [_ data]
  {:send-transaction-with-signature data})

(fx/defn sign-message-completed
  [_ signature]
  (let [signature' (-> signature
                       ; add 27 to last byte
                       ; https://github.com/ethereum/go-ethereum/blob/master/internal/ethapi/api.go#L431
                       (clojure.string/replace-first #"00$", "1b")
                       (clojure.string/replace-first #"01$", "1c")
                       (ethereum/normalized-hex))]
    {:dispatch
     [:signing/sign-message-completed (types/clj->json {:result signature'})]}))

(fx/defn on-sign-success
  {:events [:hardwallet.callback/on-sign-success]}
  [{:keys [db] :as cofx} signature]
  (log/info "[hardwallet] sign success: " signature)
  (let [transaction (get-in db [:hardwallet :transaction])
        tx-obj (select-keys transaction [:from :to :value :gas :gasPrice])]
    (fx/merge cofx
              {:db (-> db
                       (assoc-in [:hardwallet :pin :sign] [])
                       (assoc-in [:hardwallet :pin :status] nil)
                       (assoc-in [:hardwallet :hash] nil)
                       (assoc-in [:hardwallet :transaction] nil)
                       (assoc-in [:signing/sign :keycard-step] :success))}
              (clear-on-card-connected)
              (get-application-info (get-pairing db) nil)
              (if transaction
                (send-transaction-with-signature {:transaction  (types/clj->json transaction)
                                                  :signature    signature
                                                  :on-completed #(re-frame/dispatch [:signing/transaction-completed % tx-obj])})
                (sign-message-completed signature)))))

(fx/defn on-sign-error
  [{:keys [db] :as cofx} error]
  (log/debug "[hardwallet] sign error: " error)
  (let [tag-was-lost? (= "Tag was lost." (:error error))
        pinless? (= :pinless (get-in db [:signing/sign :type]))
        #_(= status-im.constants/web3-keycard-sign-typed-data
             (get-in db [:navigation/screen-params :wallet-sign-message-modal :method]))]
    (fx/merge cofx
              (when tag-was-lost?
                (fn [{:keys [db] :as cofx}]
                  (fx/merge cofx
                            {:db             (-> db
                                                 (assoc-in [:hardwallet :pin :status] nil)
                                                 (assoc-in [:signing/sign :keycard-step] (when pinless? :error :connect)))
                             :utils/show-popup {:title   (i18n/label :t/error)
                                                :content (i18n/label :t/cannot-read-card)}}
                            (set-on-card-connected (if pinless? :hardwallet/sign-typed-data
                                                       :hardwallet/prepare-to-sign)))))
              (if (re-matches pin-mismatch-error (:error error))
                (fn [{:keys [db] :as cofx}]
                  (fx/merge cofx
                            {:db (-> db
                                     (update-in [:hardwallet :pin] merge {:status      :error
                                                                          :sign        []
                                                                          :error-label :t/pin-mismatch})
                                     (assoc-in [:signing/sign :keycard-step] :pin))}
                            (get-application-info (get-pairing db) nil)))
                (show-wrong-keycard-alert true)))))

