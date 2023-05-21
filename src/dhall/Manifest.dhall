let WebAppManifest =
        https://gitlab.com/toastal/dhall-webmanifest/raw/trunk/src/WebAppManifest.dhall
      ? https://git.sr.ht/~toastal/dhall-webmanifest/blob/trunk/src/WebAppManifest.dhall

let icons =
      [ WebAppManifest.ManifestImageResource::{
        , purpose = Some "any"
        , sizes = Some "any"
        , src = "images/logo.svg"
        , type = Some "image/svg+xml"
        }
      , WebAppManifest.ManifestImageResource::{
        , purpose = Some "maskable"
        , sizes = Some "192x192"
        , src = "images/maskable_logo_192.png"
        , type = Some "image/png"
        }
      , WebAppManifest.ManifestImageResource::{
        , purpose = Some "maskable"
        , sizes = Some "512x512"
        , src = "images/maskable_logo_512.png"
        , type = Some "image/png"
        }
      ]

let makeFor =
      \(environment : ./Environment.dhall) ->
        WebAppManifest::{
        , background_color = Some "#ffffff"
        , categories = Some [ "programming" ]
        , dir = Some WebAppManifest.TextDirection.Type.ltr
        , display = Some WebAppManifest.DisplayMode.Type.standalone
        , icons
        , lang = Some "en-US"
        , name = "MLogo"
        , orientation = Some WebAppManifest.OrientationLock.Type.natural
        , scope = Some
            (merge { development = "/", production = "/mlogo" } environment)
        , start_url = Some
            (merge { development = "/", production = "/mlogo" } environment)
        , theme_color = Some "#ffffff"
        }

in  { makeFor }
