# Changelog

## [0.0.16](https://github.com/vst/hostpatrol/compare/v0.0.15...v0.0.16) (2025-01-15)


### Features

* add NTP and time sync information to host report ([a1de044](https://github.com/vst/hostpatrol/commit/a1de044aca22262575760de708d7ef90eaeacaaa))
* **website:** add time-sync information to host data tabulation ([caefd22](https://github.com/vst/hostpatrol/commit/caefd22b81140b808fe946a7f9b26ea586076b2e))
* **website:** add time-sync information to host details ([5f3c0ab](https://github.com/vst/hostpatrol/commit/5f3c0ab654de3021d46e7e6c89389fcb78ac6091))
* **website:** improve how code-blocks are rendered on pages ([c97c401](https://github.com/vst/hostpatrol/commit/c97c401959bb9efb1f69c141a0ff337e10eee694))


### Bug Fixes

* ignore DSA keygen error in case ssh-keygen does not support DSA ([31fe6ca](https://github.com/vst/hostpatrol/commit/31fe6caaaf747faf10cd64b3da5d7d0f58eb47a0))
* **website:** fix host details panel heights ([755961d](https://github.com/vst/hostpatrol/commit/755961dc0a0a38150fdb21205993db1731392b54))
* **website:** fix paddings and margins on pages layout ([e8bee74](https://github.com/vst/hostpatrol/commit/e8bee744fa830cb6e1a7f1af435cfc2e90384964))
* **website:** replace the screenshot with a smaller one ([2fb7123](https://github.com/vst/hostpatrol/commit/2fb7123ae5c3305552122695d2c361f0a98a2f68))
* **website:** show logo in hero only on large display ([bdaa2e2](https://github.com/vst/hostpatrol/commit/bdaa2e295078d34d49a9130296ecc63129fc47a3))

## [0.0.15](https://github.com/vst/hostpatrol/compare/v0.0.14...v0.0.15) (2024-04-19)


### Features

* create "Quick Start" page ([c85687b](https://github.com/vst/hostpatrol/commit/c85687b2c777b18bacf19d26ae38b153a7175d21))
* **website:** improve landing page hero, add sample screenshot ([d5bc979](https://github.com/vst/hostpatrol/commit/d5bc979f74081be8223f80dc6d93fb343e62f47c))
* **website:** revisit "Learn More" section ([f60f187](https://github.com/vst/hostpatrol/commit/f60f187e2158169b0e0c406f490b9c2af35e82bf))
* **website:** revisit navigation menu ([bfe77ad](https://github.com/vst/hostpatrol/commit/bfe77ad893ecfe7d743e1085e1ef1f215c9357db))


### Bug Fixes

* rename --parallel option to --threads for clarity ([6373e25](https://github.com/vst/hostpatrol/commit/6373e2580efb9a3c865f57ba70035f39eb8624d1))
* use Zamazingo.Time.DateTime instead of Data.Time.UTCTime ([002a869](https://github.com/vst/hostpatrol/commit/002a869181eb255a9c1c95e2422df1b1a1d36363))

## [0.0.14](https://github.com/vst/hostpatrol/compare/v0.0.13...v0.0.14) (2024-04-16)


### Features

* add report meta information ([7af041d](https://github.com/vst/hostpatrol/commit/7af041dad543a9ffc4217074f871986ec78afee3))
* make parallel patrolling the only option ([21d48e8](https://github.com/vst/hostpatrol/commit/21d48e8c6e9ae9e4882557ebd5c3c603500fbe5b))
* report errors during patrolling ([637a70b](https://github.com/vst/hostpatrol/commit/637a70b536c38fe28b85d8b7e23480a00f123f70))
* **website:** add report meta to overview component ([01cf995](https://github.com/vst/hostpatrol/commit/01cf9951a3d256435150ff58a904fc644cfa47ca))
* **website:** report errors on the overview component ([2de9b13](https://github.com/vst/hostpatrol/commit/2de9b13fced7273d1ce4f33f49dca2fe32e2d9f7))


### Bug Fixes

* **website:** fix authorized SSH key states shown on host details ([91da467](https://github.com/vst/hostpatrol/commit/91da46730891e1230b7965a5d644d4364c8b68ec))

## [0.0.13](https://github.com/vst/hostpatrol/compare/v0.0.12...v0.0.13) (2024-04-15)


### Bug Fixes

* make tests run under pure Nix shell, fix Nix installation ([955c91b](https://github.com/vst/hostpatrol/commit/955c91b0a773ba581e8761108f74f590234095d6))
* **website:** make small fixes/adjustments to the landing page content ([b663d71](https://github.com/vst/hostpatrol/commit/b663d71fe562eaf99e037ee3d480858e4e8f66e5))

## [0.0.12](https://github.com/vst/hostpatrol/compare/v0.0.11...v0.0.12) (2024-04-14)


### Features

* add host-level known SSH public keys information ([f3b25d4](https://github.com/vst/hostpatrol/commit/f3b25d4b65e15783658a4579f643a9bd46a99ba3))
* allow GitHub usernames for SSH public keys ([efddad8](https://github.com/vst/hostpatrol/commit/efddad8ec1286bf5257f928efe3b02e855b52ba3))
* report public SSH host keys on host ([5b543fe](https://github.com/vst/hostpatrol/commit/5b543fe614f86558993969a35f642e8215997559))
* **website:** add content to landing page ([5556e58](https://github.com/vst/hostpatrol/commit/5556e5884768f9f3b63c082abb07b00af455a5e7))
* **website:** adopt host-level authorized SSH keys in related views ([394efb6](https://github.com/vst/hostpatrol/commit/394efb61a2a60eff6e12a08a3d19ad00b635019c))
* **website:** improve SSH public keys section on host details component ([096dda8](https://github.com/vst/hostpatrol/commit/096dda8f89f655c54327c0952cea82ce59d1202a))
* **website:** show hardware info on host details component ([3ee0c42](https://github.com/vst/hostpatrol/commit/3ee0c42ce1250e2416619ec28c6bd93df61b515a))
* **website:** tabulate public SSH host keys on host details ([abe163c](https://github.com/vst/hostpatrol/commit/abe163cee7113238f0c91fa0ba92b80889a81961))


### Bug Fixes

* **website:** drop base-path ([64dddf1](https://github.com/vst/hostpatrol/commit/64dddf1c448b6dfdccbfebcb5d9a11b82b6496d4))

## [0.0.11](https://github.com/vst/lhp/compare/v0.0.10...v0.0.11) (2024-04-08)


### Features

* add optional data to host definition ([141ed01](https://github.com/vst/lhp/commit/141ed01eb96059638d6a782bbff89a216fff26aa))
* add optional external identifier to host definition ([57bc106](https://github.com/vst/lhp/commit/57bc106a2f96a1526c36dc620cf859de0694f62b))
* **website:** adopt new host spec field on host details page ([b6996b0](https://github.com/vst/lhp/commit/b6996b07619bb2b4a2bd96722e9e0823bb7076e7))


### Bug Fixes

* **release:** build and publish Website on release, not push to main ([67f7133](https://github.com/vst/lhp/commit/67f713356641c45fb348080218c811a27cb9bd4f))
* **release:** fetch all history for all tags and branches before build ([ae1ce55](https://github.com/vst/lhp/commit/ae1ce556b324c9447d110269e7984a9c9c791e40))

## [0.0.10](https://github.com/vst/lhp/compare/v0.0.9...v0.0.10) (2024-04-06)


### Features

* add script to build static and compressed binary for executable ([dc4cce5](https://github.com/vst/lhp/commit/dc4cce5c9b7e9b57ccf92151af520d2501822759))
* add version/build information command ([386ce83](https://github.com/vst/lhp/commit/386ce835ea46b9682b5c538a69d3e3da4049eb9a))
* allow optional SSH configuration via configuration file ([15ed034](https://github.com/vst/lhp/commit/15ed0344d67b342729e8c70f6707067caaf771f9))
* automate builds for static executable release artifact ([252c1fe](https://github.com/vst/lhp/commit/252c1fec033aa84f30451cc63d559f6a57087ad1))
* **website:** add clipboard functionality to SSH keys tabulation ([14d5f8b](https://github.com/vst/lhp/commit/14d5f8bccddfcc420a0a63d7d348a597c8dd0492))
* **website:** implement rudimentary overview page for report ([4ef95fe](https://github.com/vst/lhp/commit/4ef95fe0e408e52dca212f6da2045ffb471c9a1a))

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
