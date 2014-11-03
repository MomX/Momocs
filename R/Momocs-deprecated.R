# Courtesy for a smooth transition due to a massive renaming
# on the road to CRAN release

hpow <- function(...){
  cat(" * Deprecated, use 'cal_p' instead.\n")
  calibrate_harmonicpower(...)}

hquant <- function(...){
  cat(" * Deprecated, use 'cal_d' instead.\n")
  calibrate_deviations(...)}

hqual <- function(...){
  cat(" * Deprecated, use 'cal_v' instead.\n")
  calibrate_reconstructions(...)}

removeAsymmetry <- function(...){
  cat(" * Deprecated, use 'rm_Asym' instead.\n")
  rm_Asym(...) }

removeSymmetry <- function(...){
  cat(" * Deprecated, use 'rm_Sym' instead.\n")
  rm_Sym(...)}

defLandmarks <- function(...){
  cat(" * Deprecated, use 'def_ldk' instead.\n")
  def_ldk(...)
}

import.Conte <- function(...){
  cat(" * Deprecated, use 'import_Conte' instead.\n")
  import_Conte(...)
}

img.plot <- function(...){
  cat(" * Deprecated, use 'img_plot' instead.\n")
  img_plot(...)
}

img.plot0 <- function(...){
  cat(" * Deprecated, use 'img_plot0' instead.\n")
  img_plot0(...)
}



import.jpg <- function(...){
  cat(" * Deprecated, use 'import_jpg' instead.\n")
  import_jpg(...)
}

import.jpg1 <- function(...){
  cat(" * Deprecated, use 'import_jpg1' instead.\n")
  import_jpg1(...)
}

import.txt <- function(...){
  cat(" * Deprecated, use 'import_txt' instead.\n")
  import_txt(...)
}

lf.structure <- function(...){
  cat(" * Deprecated, use 'lf_structure' instead.\n")
  lf_structure(...)
}

coeff.sel  <- function(...){
  cat(" * Deprecated, use 'coeff_sel' instead.\n")
  coeff_sel(...)  
}

coeff.split  <- function(...){
  cat(" * Deprecated, use 'coeff_split' instead.\n")
  coeff_split(...)  
}

harm.pow  <- function(...){
  cat(" * Deprecated, use 'harm_pow' instead.\n")
  harm_pow(...)  
}

getPairs  <- function(...){
  cat(" * Deprecated, use 'get_pairs' instead.\n")
  get_pairs(...)  
}

coo.align  <- function(...){	
  cat(" * Deprecated, use 'coo_align' instead.\n")
  coo_align(...)	}

coo.aligncalliper	<- function(...){	
  cat(" * Deprecated, use 'coo_aligncalliper' instead.\n")
  coo_aligncalliper(...)	}

coo.alignxax	<- function(...){	
  cat(" * Deprecated, use 'coo_alignxax' instead.\n")
  coo_alignxax(...)	}

coo.area	<- function(...){	
  cat(" * Deprecated, use 'coo_area' instead.\n")
  coo_area(...)	}

coo.arrows	<- function(...){	
  cat(" * Deprecated, use 'coo_arrows' instead.\n")
  coo_arrows(...)	}

coo.baseline	<- function(...){	
  cat(" * Deprecated, use 'coo_baseline' instead.\n")
  coo_baseline(...)	}

coo.bookstein	<- function(...){	
  cat(" * Deprecated, use 'coo_bookstein' instead.\n")
  coo_bookstein(...)	}

coo.calliper	<- function(...){
  cat(" * Deprecated, use 'coo_calliper' instead.\n")
  coo_calliper(...)}

coo.centdist	<- function(...){	
  cat(" * Deprecated, use 'coo_centdist' instead.\n")
  coo_centdist(...)}

coo.center	<- function(...){	
  cat(" * Deprecated, use 'coo_center' instead.\n")
  coo_center(...)	}

coo.centpos	<- function(...){	
  cat(" * Deprecated, use 'coo_centpos' instead.\n")
  coo_centpos(...)	}

coo.centsize	<- function(...){	
  cat(" * Deprecated, use 'coo_centsize' instead.\n")
  coo_centsize(...)	}

coo.check	<- function(...){	
  cat(" * Deprecated, use 'coo_check' instead.\n")
  coo_check(...)	}

coo.chull	<- function(...){	
  cat(" * Deprecated, use 'coo_chull' instead.\n")
  coo_chull(...)	}

coo.circularity	<- function(...){	
  cat(" * Deprecated, use 'coo_circularity' instead.\n")
  coo_circularity(...)	}

coo.circularity.haralick	<- function(...){	
  cat(" * Deprecated, use 'coo_circularityharalick' instead.\n")
  coo_circularityharalick(...)	}

coo.circularity.norm	<- function(...){	
  cat(" * Deprecated, use 'coo_circularitynorm' instead.\n")
  coo_circularitynorm(...)	}

coo.close	<- function(...){	
  cat(" * Deprecated, use 'coo_close' instead.\n")
  coo_close(...)	}

coo.convexity	<- function(...){	
  cat(" * Deprecated, use 'coo_convexity' instead.\n")
  coo_convexity(...)	}

coo.down	<- function(...){	
  cat(" * Deprecated, use 'coo_down' instead.\n")
  coo_down(...)	}

coo.draw	<- function(...){	
  cat(" * Deprecated, use 'coo_draw' instead.\n")
  coo_draw(...)	}

coo.dxy	<- function(...){	
  cat(" * Deprecated, use 'coo_dxy' instead.\n")
  coo_dxy(...)	}

coo.eccentricity.boundingbox	<- function(...){	
  cat(" * Deprecated, use 'coo_eccentricityboundingbox' instead.\n")
  coo_eccentricityboundingbox(...)	}

coo.eccentricity.eigen	<- function(...){	
  cat(" * Deprecated, use 'coo_eccentricityeigen' instead.\n")
  coo_eccentricityeigen(...)	}

coo.elongation	<- function(...){	
  cat(" * Deprecated, use 'coo_elongation' instead.\n")
  coo_elongation(...)	}

coo.force2close	<- function(...){	
  cat(" * Deprecated, use 'coo_force2close' instead.\n")
  coo_force2close(...)	}

coo.interpolate	<- function(...){	
  cat(" * Deprecated, use 'coo_interpolate' instead.\n")
  coo_interpolate(...)	}

coo.ldk	<- function(...){	
  cat(" * Deprecated, use 'coo_ldk' instead.\n")
  coo_ldk(...)	}

coo.length	<- function(...){	
  cat(" * Deprecated, use 'coo_length' instead.\n")
  coo_length(...)	}

coo.list.panel	<- function(...){	
  cat(" * Deprecated, use 'coo_listpanel' instead.\n")
  coo_listpanel(...)	}

coo.lolli	<- function(...){	
  cat(" * Deprecated, use 'coo_lolli' instead.\n")
  coo_lolli(...)	}

coo.lw	<- function(...){	
  cat(" * Deprecated, use 'coo_lw' instead.\n")
  coo_lw(...)	}

coo.oscillo	<- function(...){	
  cat(" * Deprecated, use 'coo_oscillo' instead.\n")
  coo_oscillo(...)	}

coo.perim	<- function(...){	
  cat(" * Deprecated, use 'coo_perim' instead.\n")
  coo_perim(...)	}

coo.perim.cum	<- function(...){	
  cat(" * Deprecated, use 'coo_perimcum' instead.\n")
  coo_perimcum(...)	}

coo.perim.pts	<- function(...){	
  cat(" * Deprecated, use 'coo_perimpts' instead.\n")
  coo_perimpts(...)	}

coo.plot	<- function(...){	
  cat(" * Deprecated, use 'coo_plot' instead.\n")
  coo_plot(...)	}

coo.rectangularity	<- function(...){	
  cat(" * Deprecated, use 'coo_rectangularity' instead.\n")
  coo_rectangularity(...)	}

coo.rectilinearity	<- function(...){	
  cat(" * Deprecated, use 'coo_rectilinearity' instead.\n")
  coo_rectilinearity(...)	}

coo.rev	<- function(...){	
  cat(" * Deprecated, use 'coo_rev' instead.\n")
  coo_rev(...)	}

coo.rotate	<- function(...){	
  cat(" * Deprecated, use 'coo_rotate' instead.\n")
  coo_rotate(...)	}

coo.rotatecenter	<- function(...){	
  cat(" * Deprecated, use 'coo_rotatecenter' instead.\n")
  coo_rotatecenter(...)	}

coo.sample	<- function(...){	
  cat(" * Deprecated, use 'coo_sample' instead.\n")
  coo_sample(...)	}

coo.samplerr	<- function(...){	
  cat(" * Deprecated, use 'coo_samplerr' instead.\n")
  coo_samplerr(...)	}

coo.scale	<- function(...){	
  cat(" * Deprecated, use 'coo_scale' instead.\n")
  coo_scale(...)	}

coo.scalexy	<- function(...){	
  cat(" * Deprecated, use 'coo_scalex/y' instead.\n")
  coo_scalex(...)	}

coo.shear	<- function(...){	
  cat(" * Deprecated, use 'coo_shearx/y' instead.\n")
  coo_sheary(...)	}

coo.slide	<- function(...){	
  cat(" * Deprecated, use 'coo_slide' instead.\n")
  coo_slide(...)	}

coo.slidedirection	<- function(...){	
  cat(" * Deprecated, use 'coo_slidedirection' instead.\n")
  coo_slidedirection(...)	}

coo.smooth	<- function(...){	
  cat(" * Deprecated, use 'coo_smooth' instead.\n")
  coo_smooth(...)	}

coo.smoothcurve	<- function(...){	
  cat(" * Deprecated, use 'coo_smoothcurve' instead.\n")
  coo_smoothcurve(...)	}

coo.solidity	<- function(...){	
  cat(" * Deprecated, use 'coo_solidity' instead.\n")
  coo_solidity(...)	}

coo.tangle	<- function(...){	
  cat(" * Deprecated, use 'coo_tangle' instead.\n")
  coo_tangle(...)	}

coo.template	<- function(...){	
  cat(" * Deprecated, use 'coo_template' instead.\n")
  coo_template(...)	}

coo.theta.pts	<- function(...){	
  cat(" * Deprecated, use 'coo_thetapts' instead.\n")
  coo_thetapts(...)	}

coo.theta3	<- function(...){	
  cat(" * Deprecated, use 'coo_theta3' instead.\n")
  coo_theta3(...)	}

coo.trans	<- function(...){	
  cat(" * Deprecated, use 'coo_trans' instead.\n")
  coo_trans(...)	}

coo.unclose	<- function(...){	
  cat(" * Deprecated, use 'coo_unclose' instead.\n")
  coo_unclose(...)	}

coo.up	<- function(...){	
  cat(" * Deprecated, use 'coo_up' instead.\n")
  coo_up(...)	}

col.alpha  <- function(...){
  cat(" * Deprecated, use 'col_alpha' instead.\n")
  col_alpha(...)  
}
col.autumn <- function(...){
  cat(" * Deprecated, use 'col_autumn' instead.\n")
  col_autumn(...)  }

col.black  <- function(...){
  cat(" * Deprecated, use 'col_black' instead.\n")
  col_black(...)    
}
col.bw  <- function(...){
  cat(" * Deprecated, use 'col_bw' instead.\n")
  col_bw(...)  
}
col.cold  <- function(...){
  cat(" * Deprecated, use 'col_cold' instead.\n")
  col_cold(...)  
}
col.gallus  <- function(...){
  cat(" * Deprecated, use 'col_gallus' instead.\n")
  col_gallus(...)  
}
col.grey  <- function(...){
  cat(" * Deprecated, use 'col_grey' instead.\n")
  col_grey(...)  
}
col.heat  <- function(...){
  cat(" * Deprecated, use 'col_heat' instead.\n")
  col_heat(...)  
}
col.hot  <- function(...){
  cat(" * Deprecated, use 'col_hot' instead.\n")
  col_hot(...)  
}
col.india  <- function(...){
  cat(" * Deprecated, use 'col_india' instead.\n")
  col_india(...)  
}
col.qual  <- function(...){
  cat(" * Deprecated, use 'col_qual' instead.\n")
  col_qual(...)  
}
col.sari  <- function(...){
  cat(" * Deprecated, use 'col_sari' instead.\n")
  col_sari(...)  
}
col.solarized  <- function(...){
  cat(" * Deprecated, use 'col_solarized' instead.\n")
  col_solarized(...)  
}
col.spring  <- function(...){
  cat(" * Deprecated, use 'col_spring' instead.\n")
  col_spring(...)  
}
col.summer  <- function(...){
  cat(" * Deprecated, use 'col_summer' instead.\n")
  col_summer(...)  
}
col.summer2  <- function(...){
  cat(" * Deprecated, use 'col_summer2' instead.\n")
  col_summer2(...)  
}
col.transp  <- function(...){
  cat(" * Deprecated, use 'col_transp' instead.\n")
  col_transp(...)  
}

Manova  <- function(...){
  cat(" * Deprecated, use 'MANOVA' instead.\n")
  MANOVA(...)  
}

ManovaPW  <- function(...){
  cat(" * Deprecated, use 'MANOVA_PW' instead.\n")
  MANOVA_PW(...)  
}



