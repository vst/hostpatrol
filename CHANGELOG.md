# Changelog

## [0.0.9](https://github.com/vst/lhp/compare/v0.0.8...v0.0.9) (2024-03-31)


### Features

* add hostname and timezone to report output ([4491850](https://github.com/vst/lhp/commit/4491850cb1be458e16d4511c38417b2e8a107515))
* **website:** add report to its own page, make room for landing page ([109d778](https://github.com/vst/lhp/commit/109d7788ee9a8547548939741a470a8e9f495ff6))
* **website:** add SSH Keys tab to report ([81f9cef](https://github.com/vst/lhp/commit/81f9cefb3f17db522851b284cdd015d48b842ff0))
* **website:** adopt host's hostname and timezone ([2859ac3](https://github.com/vst/lhp/commit/2859ac35ce47846e57614b920130c00d31d51b0d))
* **website:** split report into tabs, refactor report module ([9719020](https://github.com/vst/lhp/commit/9719020030389ae40c03dbdc4f4dbc5c97722af9))


### Bug Fixes

* **website:** revisit the authorized SSH keys filter on hosts tabulation ([82b6b9e](https://github.com/vst/lhp/commit/82b6b9e3cbf5097b19d4be5c75bb8b1b1e937417))

## [0.0.8](https://github.com/vst/lhp/compare/v0.0.7...v0.0.8) (2024-03-30)


### Features

* add known SSH public keys ([0641141](https://github.com/vst/lhp/commit/06411411657fb85a5994c63646257c489695adf8))
* enrich SSH public key data definition ([cbceb86](https://github.com/vst/lhp/commit/cbceb8696613099af60b212f17a4a653080c7656))
* **website:** add more filters to hosts tabulation ([bb8b828](https://github.com/vst/lhp/commit/bb8b828ad7d2a7e395271d502bb9c41a68185f3d))
* **website:** add rudimentary filters to hosts tabulation ([286745f](https://github.com/vst/lhp/commit/286745f74b6a151f7ffadbcab049846194f8a3d0))


### Bug Fixes

* change the type of CPU count ([6093b8b](https://github.com/vst/lhp/commit/6093b8bd8e046e9fce2d3e400bcb7cb53627d0c2))
* fix how authorized SSH public keys are streamed from host ([fa9f719](https://github.com/vst/lhp/commit/fa9f719c49a1e30f95e9a386df98fee6ab7ef591))

## [0.0.7](https://github.com/vst/lhp/compare/v0.0.6...v0.0.7) (2024-03-26)


### Features

* report enabled systemd services and timers found on host ([8b38703](https://github.com/vst/lhp/commit/8b3870386af74c5314ebbe922ff6e56845d0b14d))
* **website:** list enabled systemd services and timers found on host ([ea980cd](https://github.com/vst/lhp/commit/ea980cd0eb5ec231c854c7c24cffdbd7ea034629))

## [0.0.6](https://github.com/vst/lhp/compare/v0.0.5...v0.0.6) (2024-03-25)


### Features

* report authorized SSH keys found on host ([9f204ec](https://github.com/vst/lhp/commit/9f204ec7d1809c901cb6e55bc4af6ed8dd66e9fa))
* **website:** list authorized SSH keys on host details component ([8cba4d2](https://github.com/vst/lhp/commit/8cba4d214dce375dd80a10389166d4707a5122e9))

## [0.0.5](https://github.com/vst/lhp/compare/v0.0.4...v0.0.5) (2024-03-24)


### Features

* **website:** improve host details ([1499f1c](https://github.com/vst/lhp/commit/1499f1cab3eaf0851f8fe012248cbd5ae61dd900))
* **website:** improve hosts tabulation ([e037086](https://github.com/vst/lhp/commit/e0370869c881a1c02c750a616f856cb4b1521749))


### Bug Fixes

* use RecordWildCards when building the report components ([a7e9502](https://github.com/vst/lhp/commit/a7e9502ceb5181148e9ba3315dc91fcc46d1e2c9))
* **website:** fix JSX keys ([e8a5214](https://github.com/vst/lhp/commit/e8a521410cbe9a2ae1500b2c2007a608cc7783d9))
* **website:** make sure that report data is parsed (validated) correctly ([d82f614](https://github.com/vst/lhp/commit/d82f6144019609921bff36c542efe4da24292c12))

## [0.0.4](https://github.com/vst/lhp/compare/v0.0.3...v0.0.4) (2024-03-22)


### Bug Fixes

* fix serialized field name for `Distribution#description` ([c85c06f](https://github.com/vst/lhp/commit/c85c06f15488406fe1181047742a3cd0a1f03fe8))
* revisit cloud.sh ([5574d8a](https://github.com/vst/lhp/commit/5574d8a2ba684885e96a01db1789fc2772344b48))

## [0.0.3](https://github.com/vst/lhp/compare/v0.0.2...v0.0.3) (2024-03-21)


### Features

* allow patrolling hosts as specified in a configuration file ([ab73ccb](https://github.com/vst/lhp/commit/ab73ccb879667088053c64f5e29841b26aad543f))
* **ui:** add rudimentary website with report UI ([953d88a](https://github.com/vst/lhp/commit/953d88a7c4c96da68a45006b0f62434bc0827526))


### Bug Fixes

* **website:** fix GitHub action to publish website ([8fe5ff9](https://github.com/vst/lhp/commit/8fe5ff9473974533fb2f52587e0e713ea659c3ba))

## [0.0.2](https://github.com/vst/lhp/compare/v0.0.1...v0.0.2) (2024-03-19)


### Features

* **nix:** install shell completions ([2d6afc9](https://github.com/vst/lhp/commit/2d6afc919764f83a6aba5c1cb5df8d8708d9b03a))

## 0.0.1 (2024-03-17)


### Features

* **cli:** add report and schema CLI subcommands ([888f2b1](https://github.com/vst/lhp/commit/888f2b11d9d3f686cfad5e6d69b71e02e22f8737))

## Changelog
