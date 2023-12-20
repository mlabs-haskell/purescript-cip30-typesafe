export const mkApi = eff =>
  new Proxy(
    {},
    {
      get() {
        return eff;
      }
    }
  );

export const throwingApi = obj =>
  new Proxy(
    {},
    {
      get() {
        return () => {
          throw obj;
        };
      }
    }
  );
