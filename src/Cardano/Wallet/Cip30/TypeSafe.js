export const _getErrorTagInt = err => () => {
    if (typeof err === 'object' && typeof err.code === 'number') {
        return err.code;
    }
    console.warn('Unable to extract error code from CIP-30 error, unexpected format. Rethrowing');
    throw err;
};

export const _getErrorInfoString = err => () => {
    if (typeof err === 'object' && typeof err.info === 'string') {
        return err.info;
    }
    console.warn('Unable to extract error message from CIP-30 error, unexpected format. Rethrowing');
    throw err;
};
