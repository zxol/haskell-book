data DaPhone = DaPhone [String]

convo :: [String]
convo =
  ["Wanna play 20 questions",
    "Ya",
    "U 1st haha",
    "Lol ok. Have u ever tasted alcohol lol",
    "Lol ya",
    "Wow ur cool haha. Ur turn",
    "Ok. Do u think I am pretty Lol",
    "Lol ya",
    "Haha thanks just making sure rofl ur turn"]

data Key = Key0 | Key1 | Key2 | Key3 | Key4 | Key5 | Key6 | Key7 | Key8 | Key9 | KeyStar | KeyPound
type Taps = Integer


typingInverse :: Char -> [(Key, Taps)]
typingInverse 'a' = [(Key1, 1)]
typingInverse 'a' = [(Key1, 1)]
typingInverse 'a' = [(Key1, 1)]
typingInverse 'a' = [(Key1, 1)]


main =
  do
    print "hello"
