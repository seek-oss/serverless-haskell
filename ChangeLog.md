# Changelog for Serverless Haskell

For the changes in v0.6.x, see this file on the corresponding branch.

# Unreleased

* Add authorizer parsing to proxy lambda request context
* Use `runtime: haskell` to distinguish Haskell functions. This makes it
  possible to use Haskell and other language functions in a single project.

# v0.7.5

* Add support for `sls deploy function`.

# v0.7.4

* Update serverless dependency to work with new NodeJS.

# v0.7.3

* Documentation fixes.

# v0.7.2

* Documentation fixes.

# v0.7.1

* Better display of Stack errors when building.
* Support deploying static executables.

# v0.7.0

* Default to building with Docker.
* Support LTS 12.

# v0.6.1

* Support `serverless-offline` and `serverless invoke local`.

# v0.6.0

* Automatically add necessary system library dependencies.

# v0.5.3

* Remove workaround for `amazonka` not being in Stackage LTS 11.

# v0.5.2

* Support Stackage LTS 11.

# v0.5.1

* Improve checking Haskell package version.

# v0.5.0

* Support projects in subdirectories.
* Use `http-types` where applicable.

# v0.4.3

* Documentation fixes.
* Do not try to build with Nix.

# v0.4.2

* Documentation fixes.
* Add API Gateway types.
* Check that the JS package version corresponds to the Haskell one.

# v0.4.1

* Improve Node version compatibility when packaging.
* Documentation fixes.

# v0.4.0

* Move JSON-related modules into their own namespace.
* Option to force using a Docker image when building.

# v0.3.1

* Speed up deployment by disabling JavaScript dependency checks.

# v0.3.0

* Fix JavaScript wrapper syntax.

# v0.2.1

* Documentation fixes.

# v0.2.0

* Documentation updates.
* Allow including library dependencies.
* Allow specifying arguments to the executable.

# v0.1.0

* Improve SNS and Lambda event types.

# v0.0.6

* Trim dependencies for releasing on Stackage.

# v0.0.4

* Release fixes.

# v0.0.3

* Release fixes.

# v0.0.2

* Release fixes.

# v0.0.1

* Initial release.
