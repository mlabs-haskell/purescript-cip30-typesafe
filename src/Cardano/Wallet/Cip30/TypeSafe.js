export const _getErrorTagInt = nothing => just => err => () => {
  if (typeof err === "object" && typeof err.code === "number") {
    return just(err.code);
  }
  return nothing;
};

export const _getErrorInfoString = nothing => just => err => () => {
  if (typeof err === "object" && typeof err.info === "string") {
    return just(err.info);
  }
  return nothing;
};

export const _getPaginateError = nothing => just => err => () => {
  if (typeof err === "object" && typeof err.maxSize === "number") {
    return just(err.maxSize);
  }
  return nothing;
};
