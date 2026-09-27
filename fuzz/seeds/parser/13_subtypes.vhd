package p is
  subtype s1 is integer range 0 to 7;
  subtype s2 is resolved std_logic_vector(7 downto 0);
  subtype s3 is (resolve) t;
  subtype s4 is t(open)(3 downto 0);
  subtype s5 is t range r'range;
end package;
