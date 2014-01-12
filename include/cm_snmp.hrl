%% Define wwwServiceEntry record.
-record(service, {
          index       = 1      :: non_neg_integer(),
          description = ""     :: string(),
          contact     = ""     :: string(),
          name        = ""     :: string(),
          %% FIXME: We might need to extapolate port from cowboy
          %% using `ranch:get_port/1`
          port                 :: pos_integer(),
          type        = server :: other | server | client | proxy |
                                  caching_proxy
         }).
