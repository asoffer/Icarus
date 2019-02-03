#ifndef ICARUS_ERROR_INFERENCE_FAILURE_REASON_H
#define ICARUS_ERROR_INFERENCE_FAILURE_REASON_H

enum class InferenceFailureReason {
  Inferrable,
  Hole,
  EmptyArray,
  NullPtr,
};

#endif  // ICARUS_ERROR_INFERENCE_FAILURE_REASON_H
