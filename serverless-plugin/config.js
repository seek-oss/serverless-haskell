// Constants

'use strict';

// Runtime handled by this plugin
const HASKELL_RUNTIME = 'haskell';

// Runtime used by the wrapper
const BASE_RUNTIME = 'nodejs10.x';

// Docker image used as reference
const DOCKER_IMAGE = 'lambci/lambda:' + BASE_RUNTIME;

module.exports.BASE_RUNTIME = BASE_RUNTIME;
module.exports.HASKELL_RUNTIME = HASKELL_RUNTIME;
module.exports.DOCKER_IMAGE = DOCKER_IMAGE;
