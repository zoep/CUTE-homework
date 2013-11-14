let main = 
  let in_chan = open_in "trace" in
  let parsed = Parser.parse in_chan in
    ()
