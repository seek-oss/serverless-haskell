// Constants

export type Runtime = string;

// Runtime handled by this plugin
export const HASKELL_RUNTIME: Runtime = 'haskell';

// Runtime used by the wrapper
export const BASE_RUNTIME: Runtime = 'provided';

// Docker image used as reference
export const DOCKER_IMAGE = 'lambci/lambda:' + BASE_RUNTIME;

// Docker image used for the builds. This is used even for building LTS 14
// projects, because this is the last Stack image with glibc version compatible
// to the one on the AWS Lambda environment.
export const BUILD_DOCKER_IMAGE = 'fpco/stack-build:lts-13.30';
