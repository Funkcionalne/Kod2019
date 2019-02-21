slova  :: Int -> [String]
slova 0 = [""]
slova k = [ ch:ws | ws <- slova (k-1), ch <- "ABCDEF" ]

-- *Main> slova 3
-- ["AAA","BAA","CAA","DAA","EAA","FAA","ABA","BBA","CBA","DBA","EBA","FBA","ACA","BCA","CCA","DCA","ECA","FCA","ADA","BDA","CDA","DDA","EDA","FDA","AEA","BEA","CEA","DEA","EEA","FEA","AFA","BFA","CFA","DFA","EFA","FFA","AAB","BAB","CAB","DAB","EAB","FAB","ABB","BBB","CBB","DBB","EBB","FBB","ACB","BCB","CCB","DCB","ECB","FCB","ADB","BDB","CDB","DDB","EDB","FDB","AEB","BEB","CEB","DEB","EEB","FEB","AFB","BFB","CFB","DFB","EFB","FFB","AAC","BAC","CAC","DAC","EAC","FAC","ABC","BBC","CBC","DBC","EBC","FBC","ACC","BCC","CCC","DCC","ECC","FCC","ADC","BDC","CDC","DDC","EDC","FDC","AEC","BEC","CEC","DEC","EEC","FEC","AFC","BFC","CFC","DFC","EFC","FFC","AAD","BAD","CAD","DAD","EAD","FAD","ABD","BBD","CBD","DBD","EBD","FBD","ACD","BCD","CCD","DCD","ECD","FCD","ADD","BDD","CDD","DDD","EDD","FDD","AED","BED","CED","DED","EED","FED","AFD","BFD","CFD","DFD","EFD","FFD","AAE","BAE","CAE","DAE","EAE","FAE","ABE","BBE","CBE","DBE","EBE","FBE","ACE","BCE","CCE","DCE","ECE","FCE","ADE","BDE","CDE","DDE","EDE","FDE","AEE","BEE","CEE","DEE","EEE","FEE","AFE","BFE","CFE","DFE","EFE","FFE","AAF","BAF","CAF","DAF","EAF","FAF","ABF","BBF","CBF","DBF","EBF","FBF","ACF","BCF","CCF","DCF","ECF","FCF","ADF","BDF","CDF","DDF","EDF","FDF","AEF","BEF","CEF","DEF","EEF","FEF","AFF","BFF","CFF","DFF","EFF","FFF"]
-- *Main> length $ slova 3
-- 216

-- inak, ale to iste
zoznamy :: Int -> [[Char]]    -- String == [Char]
zoznamy 0 = [""]
zoznamy k = if k == 0 then [ [] ]
            else [ ws ++ [ch] | ws <- zoznamy (k-1), ch <- ['A'..'F'] ]

-- *Main> slova 3
-- ["AAA","BAA","CAA","DAA","EAA","FAA","ABA","BBA","CBA","DBA","EBA","FBA","ACA","BCA","CCA","DCA","ECA","FCA","ADA","BDA","CDA","DDA","EDA","FDA","AEA","BEA","CEA","DEA","EEA","FEA","AFA","BFA","CFA","DFA","EFA","FFA","AAB","BAB","CAB","DAB","EAB","FAB","ABB","BBB","CBB","DBB","EBB","FBB","ACB","BCB","CCB","DCB","ECB","FCB","ADB","BDB","CDB","DDB","EDB","FDB","AEB","BEB","CEB","DEB","EEB","FEB","AFB","BFB","CFB","DFB","EFB","FFB","AAC","BAC","CAC","DAC","EAC","FAC","ABC","BBC","CBC","DBC","EBC","FBC","ACC","BCC","CCC","DCC","ECC","FCC","ADC","BDC","CDC","DDC","EDC","FDC","AEC","BEC","CEC","DEC","EEC","FEC","AFC","BFC","CFC","DFC","EFC","FFC","AAD","BAD","CAD","DAD","EAD","FAD","ABD","BBD","CBD","DBD","EBD","FBD","ACD","BCD","CCD","DCD","ECD","FCD","ADD","BDD","CDD","DDD","EDD","FDD","AED","BED","CED","DED","EED","FED","AFD","BFD","CFD","DFD","EFD","FFD","AAE","BAE","CAE","DAE","EAE","FAE","ABE","BBE","CBE","DBE","EBE","FBE","ACE","BCE","CCE","DCE","ECE","FCE","ADE","BDE","CDE","DDE","EDE","FDE","AEE","BEE","CEE","DEE","EEE","FEE","AFE","BFE","CFE","DFE","EFE","FFE","AAF","BAF","CAF","DAF","EAF","FAF","ABF","BBF","CBF","DBF","EBF","FBF","ACF","BCF","CCF","DCF","ECF","FCF","ADF","BDF","CDF","DDF","EDF","FDF","AEF","BEF","CEF","DEF","EEF","FEF","AFF","BFF","CFF","DFF","EFF","FFF"]
-- *Main> length $ slova 3
-- 216

variacie :: Int -> [String]
variacie 0 = [""]
variacie k = [ ch:ws | ch <- "ABCDEF", ws <- variacie (k-1) ]

-- *Main> variacie 3
-- ["AAA","AAB","AAC","AAD","AAE","AAF","ABA","ABB","ABC","ABD","ABE","ABF","ACA","ACB","ACC","ACD","ACE","ACF","ADA","ADB","ADC","ADD","ADE","ADF","AEA","AEB","AEC","AED","AEE","AEF","AFA","AFB","AFC","AFD","AFE","AFF","BAA","BAB","BAC","BAD","BAE","BAF","BBA","BBB","BBC","BBD","BBE","BBF","BCA","BCB","BCC","BCD","BCE","BCF","BDA","BDB","BDC","BDD","BDE","BDF","BEA","BEB","BEC","BED","BEE","BEF","BFA","BFB","BFC","BFD","BFE","BFF","CAA","CAB","CAC","CAD","CAE","CAF","CBA","CBB","CBC","CBD","CBE","CBF","CCA","CCB","CCC","CCD","CCE","CCF","CDA","CDB","CDC","CDD","CDE","CDF","CEA","CEB","CEC","CED","CEE","CEF","CFA","CFB","CFC","CFD","CFE","CFF","DAA","DAB","DAC","DAD","DAE","DAF","DBA","DBB","DBC","DBD","DBE","DBF","DCA","DCB","DCC","DCD","DCE","DCF","DDA","DDB","DDC","DDD","DDE","DDF","DEA","DEB","DEC","DED","DEE","DEF","DFA","DFB","DFC","DFD","DFE","DFF","EAA","EAB","EAC","EAD","EAE","EAF","EBA","EBB","EBC","EBD","EBE","EBF","ECA","ECB","ECC","ECD","ECE","ECF","EDA","EDB","EDC","EDD","EDE","EDF","EEA","EEB","EEC","EED","EEE","EEF","EFA","EFB","EFC","EFD","EFE","EFF","FAA","FAB","FAC","FAD","FAE","FAF","FBA","FBB","FBC","FBD","FBE","FBF","FCA","FCB","FCC","FCD","FCE","FCF","FDA","FDB","FDC","FDD","FDE","FDF","FEA","FEB","FEC","FED","FEE","FEF","FFA","FFB","FFC","FFD","FFE","FFF"]
-- *Main> length $ variacie 3
-- 216

                  
                  
