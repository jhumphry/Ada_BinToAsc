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

   -- Demonstrate coding into BASE16
   Put_Line("According to RFC4648 BASE16('foobar') = '666F6F626172'");
   Put("According to this package BASE16('foobar') = '");
   Put(Base16.To_String(String_To_Storage_Array("foobar")));
   Put_Line("'");
   New_Line;

   -- Demonstrate decoding from BASE16
   Put_Line("According to RFC4648 BASE16^{-1}('666F6F626172') = 'foobar'");
   Put("According to this package BASE16^{-1}('666F6F626172') = '");
   Put(Storage_Array_To_String(Base16.To_Bin("666F6F626172")));
   Put_Line("'");
   New_Line;

   -- Demonstrate coding into BASE64
   Put_Line("According to RFC4648 BASE64('foobar') = 'Zm9vYmFy'");
   Put("According to this package BASE16('foobar') = '");
   Put(Base64.To_String(String_To_Storage_Array("foobar")));
   Put_Line("'");
   New_Line;

   Put_Line("According to RFC4648 BASE64('foob') = 'Zm9vYg=='");
   Put("According to this package BASE16('foob') = '");
   Put(Base64.To_String(String_To_Storage_Array("foob")));
   Put_Line("'");
   New_Line;

   -- Demonstrate decoding from BASE64
   Put_Line("According to RFC4648 BASE64^{-1}('Zm8=') = 'fo'");
   Put("According to this package BASE64^{-1}('Zm8=') = '");
   Put(Storage_Array_To_String(Base64.To_Bin("Zm8=")));
   Put_Line("'");
   New_Line;

end BinToAsc_Example;
