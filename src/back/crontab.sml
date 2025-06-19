structure SlickCrontab = struct
    structure S = Slick
    
    val sc_name = "CRON-ENTRIES"
    val add_entries = case S.getShared sc_name of
        NONE => let
                val _ = S.setShared sc_name "true"
            in
                true
                
            end
        | SOME "true" => false
        | _ => false

    do (if add_entries then (
        
        S.cronNew ([0,1,2], [13,14,15,16,17,18,19], [0,1,2,3,4,5,6], "cron test 1", fn () => S.notice "TESTING CRON");
        S.cronNew ([30,31,32,33,34,35], [16,17,18,19], [0,1,2,3,4,5,6], "cron test 2", fn () => S.notice "TESTING CRON 2")
    
    ) else ())

end

