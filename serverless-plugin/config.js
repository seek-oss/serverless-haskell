// Constants

'use strict';

// Runtime handled by this plugin
const HASKELL_RUNTIME = 'haskell';

// Runtime used by the wrapper
const BASE_RUNTIME = 'nodejs12.x';

// Docker image used as reference
const DOCKER_IMAGE = 'lambci/lambda:' + BASE_RUNTIME;

// Docker image used for the builds. This is used even for building LTS 14
// projects, because this is the last Stack image with glibc version compatible
// to the one on the AWS Lambda environment.
const BUILD_DOCKER_IMAGE = 'fpco/stack-build:lts-13.30';

module.exports.BASE_RUNTIME = BASE_RUNTIME;
module.exports.HASKELL_RUNTIME = HASKELL_RUNTIME;
module.exports.DOCKER_IMAGE = DOCKER_IMAGE;
module.exports.BUILD_DOCKER_IMAGE = BUILD_DOCKER_IMAGE;
