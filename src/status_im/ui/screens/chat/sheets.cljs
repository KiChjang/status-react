(ns status-im.ui.screens.chat.sheets
  (:require [re-frame.core :as re-frame]
            [status-im.i18n :as i18n]
            [status-im.ui.components.react :as react]
            [status-im.ui.components.list-selection :as list-selection]
            [status-im.utils.universal-links.core :as universal-links]
            [status-im.multiaccounts.core :as multiaccounts]
            [status-im.ui.components.chat-icon.screen :as chat-icon]
            [status-im.ui.components.colors :as colors]
            [status-im.utils.platform :as platform]
            [status-im.ui.components.chat-icon.screen :as chat-icon.screen]
            [status-im.ui.components.list-item.views :as list-item])
  (:require-macros [status-im.utils.views :refer [defview letsubs]]))

(defn hide-sheet-and-dispatch [event]
  (re-frame/dispatch [:bottom-sheet/hide-sheet])
  (re-frame/dispatch event))

(defn view-profile [{:keys [name helper]}]
  [react/view
   [react/text {:style {:font-weight "500"
                        :line-height 22
                        :font-size   15
                        :color       :black}}
    name]
   [react/text {:style {:line-height 22
                        :font-size   15
                        :color       colors/gray}}
    (i18n/label helper)]])

(defn chat-actions [{:keys [chat-id contact chat-name]}]
  [react/view
   [list-item/list-item
    {:theme       :action
     :icon        (multiaccounts/displayed-photo contact)
     :title       [view-profile {:name   chat-name
                                 :helper :t/view-profile}]
     :accessories [:chevron]
     :on-press    #(hide-sheet-and-dispatch  [:chat.ui/show-profile chat-id])}]
   [list-item/list-item
    {:theme    :action
     :title    :t/clear-history
     :icon     :main-icons/close
     :on-press #(hide-sheet-and-dispatch [:chat.ui/clear-history-pressed chat-id])}]
   [list-item/list-item
    {:theme    :action
     :title    :t/fetch-history
     :icon     :main-icons/arrow-down
     :on-press #(hide-sheet-and-dispatch [:chat.ui/fetch-history-pressed chat-id])}]
   [list-item/list-item
    {:theme    :action-destructive
     :title    :t/delete-chat
     :icon     :main-icons/delete
     :on-press #(hide-sheet-and-dispatch [:chat.ui/remove-chat-pressed chat-id])}]])

(defn public-chat-actions [{:keys [chat-id]}]
  (let [link    (universal-links/generate-link :public-chat :external chat-id)
        message (i18n/label :t/share-public-chat-text {:link link})]
    [react/view
     (when-not platform/desktop?
       [list-item/list-item
        {:theme    :action
         :title    :t/share-chat
         :icon     :main-icons/share
         :on-press (fn []
                     (re-frame/dispatch [:bottom-sheet/hide-sheet])
                     (list-selection/open-share {:message message}))}])
     [list-item/list-item
      {:theme    :action
       :title    :t/clear-history
       :icon     :main-icons/close
       :on-press #(hide-sheet-and-dispatch [:chat.ui/clear-history-pressed chat-id])}]
     [list-item/list-item
      {:theme    :action
       :title    :t/fetch-history
       :icon     :main-icons/arrow-down
       :on-press #(hide-sheet-and-dispatch [:chat.ui/fetch-history-pressed chat-id])}]
     [list-item/list-item
      {:theme    :action-destructive
       :title    :t/delete-chat
       :icon     :main-icons/delete
       :on-press #(hide-sheet-and-dispatch [:group-chats.ui/remove-chat-pressed chat-id])}]]))

(defn group-chat-actions [{:keys [chat-id contact group-chat chat-name color online]}]
  [react/view
   [list-item/list-item
    {:theme       :action
     :title       [view-profile {:name   chat-name
                                 :helper :t/group-info}]
     :icon        [chat-icon/chat-icon-view-chat-sheet
                   contact group-chat chat-name color online]
     :accessories [:chevron]
     :on-press    #(hide-sheet-and-dispatch [:show-group-chat-profile chat-id])}]
   [list-item/list-item
    {:theme    :action
     :title    :t/clear-history
     :icon     :main-icons/close
     :on-press #(hide-sheet-and-dispatch [:chat.ui/clear-history-pressed chat-id])}]
   [list-item/list-item
    {:theme    :action
     :title    :t/fetch-history
     :icon     :main-icons/arrow-down
     :on-press #(hide-sheet-and-dispatch [:chat.ui/fetch-history-pressed chat-id])}]
   [list-item/list-item
    {:theme    :action-destructive
     :title    :t/delete-chat
     :icon     :main-icons/delete
     :on-press #(hide-sheet-and-dispatch [:group-chats.ui/remove-chat-pressed chat-id])}]])

(defn actions [{:keys [public? group-chat]
                :as current-chat}]
  (cond
    public?    [public-chat-actions current-chat]
    group-chat [group-chat-actions current-chat]
    :else      [chat-actions current-chat]))
