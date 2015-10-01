-- RFC4648
-- Binary data to ASCII codecs specified in RFC4648

-- Copyright (c) 2015, James Humphry - see LICENSE file for details

with System.Storage_Elements;
with BinToAsc, BinToAsc.Base16, BinToAsc.Base32, BinToAsc.Base64;

package RFC4648 is

   package BToA is new BinToAsc(Bin => System.Storage_Elements.Storage_Element,
                                Bin_Array_Index => System.Storage_Elements.Storage_Offset,
                                Bin_Array => System.Storage_Elements.Storage_Array);

   subtype Codec_State is BToA.Codec_State;
   function Ready return BToA.Codec_State renames BToA.Ready;
   function Complete return BToA.Codec_State renames BToA.Complete;
   function Failed return BToA.Codec_State renames BToA.Failed;

   Base16_Alphabet : constant BToA.Alphabet_16 := "0123456789ABCDEF";
   package Base16 is new BToA.Base16(Alphabet       => Base16_Alphabet,
                                     Case_Sensitive => True);
   package Hex renames Base16;

   Base32_Alphabet : constant BToA.Alphabet_32 :=
     "ABCDEFGHIJKLMNOPQRSTUVWXYZ234567";
   package Base32 is new BToA.Base32(Alphabet => Base32_Alphabet,
                                     Padding  => '=');

   Base32Hex_Alphabet : constant BToA.Alphabet_32 :=
     "0123456789ABCDEFGHIJKLMNOPQRSTUV";
   package Base32Hex is new BToA.Base32(Alphabet => Base32Hex_Alphabet,
                                        Padding  => '=');

   Base64_Alphabet : constant BToA.Alphabet_64 :=
     "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/";

   package Base64 is new BToA.Base64(Alphabet => Base64_Alphabet,
                                     Padding  => '=');

   Base64_URL_Safe_Alphabet : constant BToA.Alphabet_64 :=
     "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789-_";

   package Base64_URL_Safe is new BToA.Base64(Alphabet =>
                                                 Base64_URL_Safe_Alphabet,
                                              Padding  => '=');

end RFC4648;
