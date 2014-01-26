%% Define wwwServiceEntry record.
-record(cm_svc, {
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

-record(cm_req, {
          svc_index  :: non_neg_integer(),
          type       :: cm_ww_mib:request_type(),
          size       :: non_neg_integer(),
          timestamp  :: calendar:date_time()
         }).
