-- RFC4648
-- Binary data to ASCII codecs specified in RFC4648

-- Copyright (c) 2015, James Humphry - see LICENSE file for details

with System.Storage_Elements;
with BinToAsc, BinToAsc.Base16;

package RFC4648 is

   package BToA is new BinToAsc(Bin => System.Storage_Elements.Storage_Element,
                                Bin_Array_Index => System.Storage_Elements.Storage_Offset,
                                Bin_Array => System.Storage_Elements.Storage_Array);

   subtype Codec_State is BToA.Codec_State;
   function Ready return BToA.Codec_State renames BToA.Ready;
   function Complete return BToA.Codec_State renames BToA.Complete;
   function Failed return BToA.Codec_State renames BToA.Failed;

   package Base16 is new BToA.Base16; -- default generic parameters are suitable

end RFC4648;
