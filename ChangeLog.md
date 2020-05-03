# Changelog for Serverless Haskell

For the changes in v0.6.x, see this file on the corresponding branch.

## Unreleased changes

* Fix deployment (#136).

## 0.11.0

* Change to a native Haskell runtime (#130). The Haskell process is now
  receiving the events from AWS and sending the results back without the
  JavaScript wrapper.
* Several Lambda invocations can reuse the same Haskell process instead of
  a new one being created each time.
* Passing arguments to the executable is not supported anymore.
* Local invocation (`sls invoke local` and `sls offline`) use the Docker build
  by default.
* `--useDocker` flag is required when using Serverless Offline.

## 0.10.5

* Fix uploading the package to NPM (#132).

## 0.10.4

* Fix uploading the package to NPM (#131).

## 0.10.3

* Fix tests of serverless-offline functionality (#128).
* Use TypeScript for the wrapper (#126).

## 0.10.2

* Speed up checking required glibc version, avoiding potentially very long
  deployment times (#124).
* Fix running integration tests on macOS (#125).

## 0.10.1

* Support LTS 15.

## 0.10.0

* Adds support for SQS events, including those with embedded SNS and S3 messages.

## 0.9.4

* Update Node.js runtime to 12.x.

## 0.9.3

* More robust deployment process.

## 0.9.2

* Support LTS 14.
* Check glibc version of the resulting executable to avoid errors when running
  (#114).
* Use LTS 13 Docker image for building the binary to avoid depending on glibc
  version not present on AWS.

## 0.9.1

* Release a version following the proper release process.

## 0.9.0

* Use `ObjectVersionId` from `amazonka-s3` in S3 events.

## 0.8.11

* Update the list of system libraries available on AWS Lambda (#108).

## 0.8.10

* Fix deployment to Hackage using new Stack.

## 0.8.9

* Close the listening socket to fix local invocations hanging (#103).

## 0.8.8

* Switch AWS Lambda runtime to NodeJS 10.x.

## 0.8.7

* Build the whole project instead of individual handlers by default for better
  caching behavior.

## 0.8.6

* Ensure Serverless variable substitutions properly affect the `docker` value
  in the configuration.

## 0.8.5

* Explicitly support LTS 13

## 0.8.4

* Change the communication method between the JavaScript wrapper and the Haskell
  process to TCP. This fixes errors like "Resource vanished: broken pipe" when
  running the Haskell executable standalone.
* Do not crash when `ldd` is not found when locally invoking a function on macOS.
* Fix error message about no Haskell functions being found when invoking a
  function locally.

## 0.8.3

* It is now an error if the plugin is enabled but there are no functions with
  Haskell runtime.

## 0.8.2

* Add authorizer parsing to proxy lambda request context
* Use `runtime: haskell` to distinguish Haskell functions. This makes it
  possible to use Haskell and other language functions in a single project.

## v0.7.5

* Add support for `sls deploy function`.

## v0.7.4

* Update serverless dependency to work with new NodeJS.

## v0.7.3

* Documentation fixes.

## v0.7.2

* Documentation fixes.

## v0.7.1

* Better display of Stack errors when building.
* Support deploying static executables.

## v0.7.0

* Default to building with Docker.
* Support LTS 12.

## v0.6.1

* Support `serverless-offline` and `serverless invoke local`.

## v0.6.0

* Automatically add necessary system library dependencies.

## v0.5.3

* Remove workaround for `amazonka` not being in Stackage LTS 11.

## v0.5.2

* Support Stackage LTS 11.

## v0.5.1

* Improve checking Haskell package version.

## v0.5.0

* Support projects in subdirectories.
* Use `http-types` where applicable.

## v0.4.3

* Documentation fixes.
* Do not try to build with Nix.

## v0.4.2

* Documentation fixes.
* Add API Gateway types.
* Check that the JS package version corresponds to the Haskell one.

## v0.4.1

* Improve Node version compatibility when packaging.
* Documentation fixes.

## v0.4.0

* Move JSON-related modules into their own namespace.
* Option to force using a Docker image when building.

## v0.3.1

* Speed up deployment by disabling JavaScript dependency checks.

## v0.3.0

* Fix JavaScript wrapper syntax.

## v0.2.1

* Documentation fixes.

## v0.2.0

* Documentation updates.
* Allow including library dependencies.
* Allow specifying arguments to the executable.

## v0.1.0

* Improve SNS and Lambda event types.

## v0.0.6

* Trim dependencies for releasing on Stackage.

## v0.0.4

* Release fixes.

## v0.0.3

* Release fixes.

## v0.0.2

* Release fixes.

## v0.0.1

* Initial release.
