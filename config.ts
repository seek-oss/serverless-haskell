// Constants

export type Runtime = string;

// Runtime handled by this plugin
export const HASKELL_RUNTIME: Runtime = 'haskell';

// Runtime used by the wrapper
export const BASE_RUNTIME: Runtime = 'provided.al2';

// Docker image used as reference
// https://aws.amazon.com/blogs/aws/new-for-aws-lambda-container-image-support/
// https://hub.docker.com/r/amazon/aws-lambda-provided
export const DOCKER_IMAGE = 'amazon/aws-lambda-provided:al2';

// Docker image used for the builds. This is used even for building LTS 14
// projects, because this is the last Stack image with glibc version compatible
// to the one on the AWS Lambda environment.
export const BUILD_DOCKER_IMAGE = 'fpco/stack-build:lts-13.30';
