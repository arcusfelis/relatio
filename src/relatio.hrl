%% -----------------------------------------------------------------------
%% Record Definitions
%% -----------------------------------------------------------------------

-record(r_node_set, {
        id :: relatio_type:node_set_id(),
        value :: relatio_type:node_set_value()
}).

-record(r_counter, {
        table           :: relatio_type:table_name(),
        last_id         :: relatio_type:record_id()
}).

