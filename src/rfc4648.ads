-- RFC4648
-- Binary data to ASCII codecs specified in RFC4648

-- Copyright (c) 2015, James Humphry - see LICENSE file for details

with System.Storage_Elements;
with BinToAsc, BinToAsc.Base16, BinToAsc.Base64;

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

   Base64_Alphabet : constant BToA.Alphabet_64 :=
     "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/";

   package Base64 is new BToA.Base64(Alphabet => Base64_Alphabet,
                                     Padding  => '=');



end RFC4648;
