-- BinToAsc_Example
-- Binary data to/from ASCII codecs example

-- Copyright (c) 2015, James Humphry - see LICENSE file for details

with Ada.Text_IO;
use Ada.Text_IO;

with String_To_Storage_Array, Storage_Array_To_String;

with RFC4648;
use RFC4648;

procedure BinToAsc_Example is
begin

   Put_Line("Examples of using the BinToAsc package.");
   New_Line;

   -- Demonstrate coding into Base16
   Put_Line("According to RFC4648 Base16('foobar') = '666F6F626172'");
   Put("According to this package Base16('foobar') = '");
   Put(Base16.To_String(String_To_Storage_Array("foobar")));
   Put_Line("'");
   New_Line;

   -- Demonstrate decoding from Base16
   Put_Line("According to RFC4648 Base16^{-1}('666F6F626172') = 'foobar'");
   Put("According to this package Base16^{-1}('666F6F626172') = '");
   Put(Storage_Array_To_String(Base16.To_Bin("666F6F626172")));
   Put_Line("'");
   New_Line;

   -- Demonstrate coding into Base32
   Put_Line("According to RFC4648 Base32('foobar') = 'MZXW6YTBOI======'");
   Put("According to this package Base32('foobar') = '");
   Put(Base32.To_String(String_To_Storage_Array("foobar")));
   Put_Line("'");
   New_Line;

   -- Demonstrate decoding from Base32
   Put_Line("According to RFC4648 Base32^{-1}('MZXW6YTBOI======') = 'foobar'");
   Put("According to this package Base32^{-1}('MZXW6YTBOI======') = '");
   Put(Storage_Array_To_String(Base32.To_Bin("MZXW6YTBOI======")));
   Put_Line("'");
   New_Line;

   -- Demonstrate decoding from Base32 with homoglyphs
   Put_Line("According to RFC4648 with homoglphs permitted Base32^{-1}('MZXW6YTB01======') = 'foobar'");
   Put("According to this package with homoglphs permitted Base32^{-1}('MZXW6YTB01======') = '");
   Put(Storage_Array_To_String(Base32_Allow_Homoglyphs.To_Bin("MZXW6YTB01======")));
   Put_Line("'");
   New_Line;

   -- Demonstrate coding into Base32Hex
   Put_Line("According to RFC4648 Base32Hex('foobar') = 'CPNMUOJ1E8======'");
   Put("According to this package Base32Hex('foobar') = '");
   Put(Base32Hex.To_String(String_To_Storage_Array("foobar")));
   Put_Line("'");
   New_Line;

   -- Demonstrate decoding from Base32Hex
   Put_Line("According to RFC4648 Base32Hex^{-1}('CPNMUOJ1E8======') = 'foobar'");
   Put("According to this package Base32Hex^{-1}('CPNMUOJ1E8======') = '");
   Put(Storage_Array_To_String(Base32Hex.To_Bin("CPNMUOJ1E8======")));
   Put_Line("'");
   New_Line;

   -- Demonstrate coding into Base64
   Put_Line("According to RFC4648 Base64('foobar') = 'Zm9vYmFy'");
   Put("According to this package Base16('foobar') = '");
   Put(Base64.To_String(String_To_Storage_Array("foobar")));
   Put_Line("'");
   New_Line;

   Put_Line("According to RFC4648 Base64('foob') = 'Zm9vYg=='");
   Put("According to this package Base16('foob') = '");
   Put(Base64.To_String(String_To_Storage_Array("foob")));
   Put_Line("'");
   New_Line;

   -- Demonstrate decoding from Base64
   Put_Line("According to RFC4648 Base64^{-1}('Zm8=') = 'fo'");
   Put("According to this package Base64^{-1}('Zm8=') = '");
   Put(Storage_Array_To_String(Base64.To_Bin("Zm8=")));
   Put_Line("'");
   New_Line;

end BinToAsc_Example;
