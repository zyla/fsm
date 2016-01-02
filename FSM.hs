cmd = [0, 0, 1, 1]

dac :: Signal (Vec 12 Bit) -> Seq
dac input = do
  repeat 4 $ \index -> output (0,1,0,cmd !! index)
  repeat 12 $ \bit -> output (0,1,0,bit)
  output (0,1,1,0)
