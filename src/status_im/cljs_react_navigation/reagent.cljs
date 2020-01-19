(ns status-im.cljs-react-navigation.reagent
  (:require [status-im.cljs-react-navigation.base :as base]
            [reagent.core :as r]
            [reagent.impl.component :as ric]
            [oops.core :refer [ocall]]
            [cljs.spec.alpha :as s :include-macros true]))

(defn react-component?
  "Same as base, but now accepts a reagent component fn"
  [c]
  (cond
    (ric/react-class? c) c
    (fn? c) (r/reactify-component
              (fn [props & children]
                [c (js->clj props :keywordize-keys true) children]))
    :else :cljs.spec.alpha/invalid))

(defn react-element?
  "Same as base, but now accepts a reagent component fn"
  [e]
  (cond
    (base/isValidElement e) e
    (ric/react-class? e) (r/create-element e)
    (fn? e) (r/create-element
              (r/reactify-component
                (fn [props & children]
                  [e (js->clj props :keywordize-keys true) children])))
    :else :cljs.spec.alpha/invalid))

(defn fn-or-react-component?
  "Same as base, but now *expects* a reagent component if a fn is supplied"
  [fn-or-c]
  (cond
    (ric/react-class? fn-or-c) fn-or-c
    (fn? fn-or-c) (fn [props & children]
                    (let [clj-props (js->clj props :keywordize-keys true)]
                      (r/reactify-component (fn-or-c clj-props children))))
    :else :cljs.spec.alpha/invalid))

(defn fn-or-react-element?
  "Same as base, but now *expects* a reagent component if a fn is supplied"
  [fn-or-e]
  (cond
    (base/isValidElement fn-or-e) fn-or-e
    (ric/react-class? fn-or-e) (r/create-element fn-or-e)
    (fn? fn-or-e) (fn [props & children]
                    (let [clj-props (js->clj props :keywordize-keys true)]
                      (r/as-element [fn-or-e clj-props children])))
    :else :cljs.spec.alpha/invalid))

(defn string-or-react-element? [s-or-e]
  (cond
    (base/isValidElement s-or-e) s-or-e
    (ric/react-class? s-or-e) (r/create-element s-or-e)
    (fn? s-or-e) (r/as-element [(fn [props & children]
                                  (let [clj-props (js->clj props :keywordize-keys true)]
                                    [s-or-e clj-props children]))])
    (string? s-or-e) s-or-e
    :else :cljs.spec.alpha/invalid))

;; Spec overrides for Reagent Components
(s/def :react/component (s/conformer react-component?))
(s/def :react/element (s/conformer react-element?))
(s/def :react-navigation.navigationOptions/headerTitle (s/conformer string-or-react-element?))
(s/def :react-navigation.navigationOptions/headerLeft (s/conformer string-or-react-element?))
(s/def :react-navigation.navigationOptions/headerRight (s/conformer string-or-react-element?))
(s/def :react-navigation.navigationOptions/tabBarIcon (s/conformer fn-or-react-element?))
(s/def :react-navigation.RouteConfigs.route/screen (s/conformer fn-or-react-component?))

;; API
(def stack-screen base/stack-screen)
(def tab-screen base/tab-screen)
(def stack-navigator base/stack-navigator)
(def bottom-tab-navigator base/bottom-tab-navigator)
(def switch-navigator base/switch-navigator)
(def twopane-navigator base/twopane-navigator)
(def stack-actions base/StackActions)
(def navigation-actions base/NavigationActions)
(def navigation-events base/NavigationEvents)

(defn create-app-container [app-navigator]
  (r/adapt-react-class (base/createAppContainer app-navigator)))

(defonce navigator-ref (atom nil))

(defn set-navigator-ref [ref]
  (reset! navigator-ref ref))

(defn can-be-called? []
  (boolean @navigator-ref))

(defn navigate-to [route params]
  (when (can-be-called?)
    (ocall @navigator-ref "dispatch"
           (ocall navigation-actions "navigate"
                  #js {:routeName (name route)
                       :params    (clj->js params)}))))

(defn- navigate [params]
  (when (can-be-called?)
    (ocall navigation-actions "navigate" (clj->js params))))

(defn navigate-reset [state]
  (when (can-be-called?)
    (let [state' (update state :actions #(mapv navigate %))]
      (ocall @navigator-ref "dispatch"
             (ocall stack-actions "reset"
                    (clj->js state'))))))

(defn navigate-back []
  (when (can-be-called?)
    (ocall @navigator-ref "dispatch"
           (ocall navigation-actions "back"))))
