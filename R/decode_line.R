#' @import bitops

decode_line = function(encoded){

  vlen = nchar(encoded)
  vindex = 0
  varray = NULL
  vlat = 0
  vlng = 0

  while(vindex < vlen) {
    vb = NULL
    vshift = 0
    vresult = 0
    repeat{
      if(vindex + 1 <= vlen) {
        vindex = vindex + 1
        vb = as.integer(charToRaw(substr(encoded, vindex, vindex))) - 63
      }

      vresult = bitOr(vresult, bitShiftL(bitAnd(vb, 31), vshift))
      vshift = vshift + 5
      if(vb < 32) break
    }

    dlat = ifelse(
      bitAnd(vresult, 1),
      -(bitShiftR(vresult, 1) + 1),
      bitShiftR(vresult, 1)
    )
    vlat = vlat + dlat

    vshift = 0
    vresult = 0
    repeat{
      if(vindex + 1 <= vlen) {
        vindex = vindex + 1
        vb = as.integer(charToRaw(substr(encoded, vindex, vindex))) - 63
      }

      vresult = bitOr(vresult, bitShiftL(bitAnd(vb, 31), vshift))
      vshift = vshift + 5
      if(vb < 32) break
    }

    dlng = ifelse(
      bitAnd(vresult, 1),
      -(bitShiftR(vresult, 1) + 1),
      bitShiftR(vresult, 1)
    )
    vlng = vlng + dlng

    varray = rbind(varray, c(vlng * 1e-5, vlat * 1e-5))
  }
  colnames(varray) = c("lon", "lat")
  varray
}

