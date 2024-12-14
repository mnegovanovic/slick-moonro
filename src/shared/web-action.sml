structure WebAction = struct
    open Utils
    
    datatype result = Result of {
        ok:     bool,
        msg:    string,
        errs:   (string * string) list
    }

    fun dump (Result res): string =
        let
            val ok = Bool.toString (#ok res)
            val msg = urlencode (#msg res)
            val errs = List.foldl (fn ((k,v), acc) => acc^(urlencode k)^" "^(urlencode v)^"\n") "" (#errs res)
        in
            ok^"\n"^msg^"\n"^errs
        end

    fun load (repr: string): result =
        let
            val lines = String.fields (fn c => c = #"\n") repr
            val lines = List.filter (fn p => p <> "") lines
            
            datatype state = OK_LINE | MSG_LINE | ERRS
            val state = ref OK_LINE

            val ok = ref false
            val msg = ref ""
            val errs: (string * string) list ref = ref []
            
            fun process_ [] = ()
                | process_ (l::lines) =
                    case !state of
                        OK_LINE =>
                            let in
                                ok := valOf (Bool.fromString l);
                                state := MSG_LINE;
                                process_ lines
                            end
                        | MSG_LINE =>
                            let in
                                msg := urldecode l;
                                state := ERRS;
                                process_ lines
                            end
                        | ERRS =>
                            let
                                val parts = String.fields (fn c => c = #" ") l
                                val k = urldecode (List.nth (parts, 0))
                                val v = urldecode (List.nth (parts, 1))
                            in
                                errs := (k,v)::(!errs);
                                process_ lines
                            end
        in
            process_ lines;
            Result {
                ok = !ok,
                msg = !msg,
                errs = !errs}
        end
end

