// Constants

export type Runtime = string;

// Runtime handled by this plugin
export const HASKELL_RUNTIME: Runtime = 'haskell';

// Runtime used by the wrapper
export const BASE_RUNTIME: Runtime = 'provided.al2023';

// Docker image used as reference
// https://aws.amazon.com/blogs/aws/new-for-aws-lambda-container-image-support/
// https://hub.docker.com/r/amazon/aws-lambda-provided
export const DOCKER_IMAGE = 'amazon/aws-lambda-provided:al2023';

// Docker image used for the builds. This needs to have all dependencies for
// running GHC and Stack, but can't have glibc later what AWS provides.
export const BUILD_DOCKER_IMAGE = 'haskell:stretch';
