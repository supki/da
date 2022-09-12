{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module TH
  ( key
  ) where

import Crypto.PubKey.RSA (PublicKey(PublicKey, public_size, public_n, public_e))
import Data.PEM (PEM(PEM, pemContent), pemParseBS)
import Data.X509 (PubKey(PubKeyRSA), certPubKey, getCertificate, decodeSignedCertificate)


key :: PublicKey
key = $(
  either error id $ do
    let raw =
          "-----BEGIN CERTIFICATE-----\n\
          \MIIHqDCCBpCgAwIBAgIRAJTVuE4m/2NQqOjSxDv6nFowDQYJKoZIhvcNAQEFBQAw\n\
          \gY4xCzAJBgNVBAYTAkdCMRswGQYDVQQIExJHcmVhdGVyIE1hbmNoZXN0ZXIxEDAO\n\
          \BgNVBAcTB1NhbGZvcmQxGjAYBgNVBAoTEUNPTU9ETyBDQSBMaW1pdGVkMTQwMgYD\n\
          \VQQDEytDT01PRE8gRXh0ZW5kZWQgVmFsaWRhdGlvbiBTZWN1cmUgU2VydmVyIENB\n\
          \MB4XDTE0MDExNzAwMDAwMFoXDTE2MDExNzIzNTk1OVowggEWMQ8wDQYDVQQFEwY0\n\
          \ODk0MzQxEzARBgsrBgEEAYI3PAIBAxMCSUUxHTAbBgNVBA8TFFByaXZhdGUgT3Jn\n\
          \YW5pemF0aW9uMQswCQYDVQQGEwJJRTEOMAwGA1UEERMFMDAwMDQxEzARBgNVBAgT\n\
          \CkNvLiBEdWJsaW4xDzANBgNVBAcTBkR1YmxpbjEYMBYGA1UECRMPMiBXYXRlcmxv\n\
          \byBSb2FkMRAwDgYDVQQJEwdVbml0IDIwMTswOQYDVQQKEzJSQU5ET00uT1JHIChS\n\
          \YW5kb21uZXNzIGFuZCBJbnRlZ3JpdHkgU2VydmljZXMgTHRkKTEjMCEGA1UECxMa\n\
          \Q09NT0RPIEVWIE11bHRpLURvbWFpbiBTU0wwggIiMA0GCSqGSIb3DQEBAQUAA4IC\n\
          \DwAwggIKAoICAQDs7cdBYudPMIKP+rCgji+P9P3bfvB7vivBwlbbDhK7MgpWUCfn\n\
          \KFolxp5Cl2mYfCZC3dpTwbVtruffGXuF14+SH5oSRgzeJU6Ell2QIqPPDbHuVRJA\n\
          \idmSyCezxHiIaSUk8idfp+YGMSu3ViuMjwHkerPeSiJuSohmBW5nVB8miBuaytPr\n\
          \iKaCIN14bdcNw5jjIPNLvfhs2pFQ1iFrdoOfC/Gu5vIyF9a0GXbLqdcoNt4won01\n\
          \a7vbdXsv4EYV4S9gw+ryJ5FUnvJxq8p5JcSiL0a+DMKO7LYYEk5ezjU7l/TtWeob\n\
          \FyLq6rJuUSCvRKg0RNgWcmxJWSvLJM+07uWHmN0WDhCYcFQR/N9xZAyTGPgtsO9E\n\
          \cyflQiuh+QDuD73tZ/8hCdnOGVmH4OAhveONcPnQaomx3tx3SiMlm7MZ/oEtJnyD\n\
          \Ypk4ncq0HW7+dngdVBR0/pk2ineYTHsyJqvvBIONHMaDhrJ/Edryk60Tqjyl7R3u\n\
          \VW7ddMcL2QvmpndeqV3pLH20nZlDagONM+U8iFgYwt14SFeZhSuGcMKGk4mta+xv\n\
          \96HgzfyxZRxwFBOX2wG9ZGSttIJrOXFkD5jko48Qnc0hHwaMoU3Bt3wGT1iTcudu\n\
          \hxKncTzYFUPWCLjNF30y0GEKUZz//GLxLlasWGjyX6xn50Kr+K5VgtOQZQIDAQAB\n\
          \o4ICdDCCAnAwHwYDVR0jBBgwFoAUiERR/1AqaV4tiPQhutkM8s7L6nwwHQYDVR0O\n\
          \BBYEFBi2i0szTLpH4n+56NXdzhuXTn0HMA4GA1UdDwEB/wQEAwIFoDAMBgNVHRMB\n\
          \Af8EAjAAMB0GA1UdJQQWMBQGCCsGAQUFBwMBBggrBgEFBQcDAjBGBgNVHSAEPzA9\n\
          \MDsGDCsGAQQBsjEBAgEFATArMCkGCCsGAQUFBwIBFh1odHRwczovL3NlY3VyZS5j\n\
          \b21vZG8uY29tL0NQUzBTBgNVHR8ETDBKMEigRqBEhkJodHRwOi8vY3JsLmNvbW9k\n\
          \b2NhLmNvbS9DT01PRE9FeHRlbmRlZFZhbGlkYXRpb25TZWN1cmVTZXJ2ZXJDQS5j\n\
          \cmwwgYQGCCsGAQUFBwEBBHgwdjBOBggrBgEFBQcwAoZCaHR0cDovL2NydC5jb21v\n\
          \ZG9jYS5jb20vQ09NT0RPRXh0ZW5kZWRWYWxpZGF0aW9uU2VjdXJlU2VydmVyQ0Eu\n\
          \Y3J0MCQGCCsGAQUFBzABhhhodHRwOi8vb2NzcC5jb21vZG9jYS5jb20wgcwGA1Ud\n\
          \EQSBxDCBwYIOYXBpLnJhbmRvbS5vcmeCEGNvZGVzLnJhbmRvbS5vcmeCEGRyYXdz\n\
          \LnJhbmRvbS5vcmeCEGZlZWRzLnJhbmRvbS5vcmeCEGZpbGVzLnJhbmRvbS5vcmeC\n\
          \EGdhbWVzLnJhbmRvbS5vcmeCEnJhZmZsZXMucmFuZG9tLm9yZ4IKcmFuZG9tLm9y\n\
          \Z4IRdHVwbGVzLnJhbmRvbS5vcmeCEndpZGdldHMucmFuZG9tLm9yZ4IOd3d3LnJh\n\
          \bmRvbS5vcmcwDQYJKoZIhvcNAQEFBQADggEBACN5Vy82UQaPGFXVZCHBJ+gVl77J\n\
          \sf8pDVZcNTWvj7yi8erwGSXl5QYridURuqbcQFrgcmbCtEmJ9xGHiYugAurpdtoh\n\
          \DbPm+1ISSemLdhiL8nXaT0HYazQyRSPooXI7ZZcN1S07KCjtcpp9aRP5Qm5gx04q\n\
          \V4F81n1b7atKVZbWFTBunYzWteo8rusc30MK2fIBxKG/gW2hKA/USubgb5roCO1f\n\
          \KKu2/B5BuDPeEMG92CTAg9C2xAe6EloGJSnYENk+4OKnNGp+HkEN6abhLETNE/IQ\n\
          \pZ6BmNVZQyjSYasDmRQeAmO7pjG7oqQrWyrwYJsJr0/G1aWzaaV3PDS7NjI=\n\
          \-----END CERTIFICATE-----"
    ~([PEM { pemContent }])
      <- pemParseBS raw
    ~(PubKeyRSA PublicKey { public_size, public_n, public_e })
      <- fmap (certPubKey . getCertificate) (decodeSignedCertificate pemContent)
    return [| PublicKey { public_size, public_n, public_e } |])
