library(tidyr)
library(lavaan)
##### for SEM
mod<-'
  FSES =~ ME+PE+PO+MO+FR+ITN+UrS
  ASES =~FID+FUS+FIC+HI
  CSES =~ EE+GDP+HB
  SC =~ a*MCA+a*FAS
  AP=~ b*NO2+b*PM2.5

  LFA =~ CST_L+CST_R+DRTT_L+DRTT_R+RST_L+CBT_L
  LReHo =~ ReHo_ITG_R+ReHo_ITG_L+ReHo_MOFC
  CBRGMV =~ PCL_R+ITG_L+MPFC
  CBLGMV =~ CPL_L+CPL_R
  
FSES~o1*OC
ASES~o2*OC
CSES~o3*OC
SC~o4*OC
AP~o5*OC

LFA~c1*OC+i1e1*FSES+i1e2*ASES+i1e3*CSES+i1e4*SC+i1e5*AP
LReHo~c2*OC+i2e1*FSES+i2e2*ASES+i2e3*CSES+i2e4*SC+i2e5*AP
CBRGMV~c3*OC+i3e1*FSES+i3e2*ASES+i3e3*CSES+i3e4*SC+i3e5*AP
CBLGMV~c4*OC+i4e1*FSES+i4e2*ASES+i4e3*CSES+i4e4*SC+i4e5*AP
PAT_R~c5*OC+i5e1*FSES+i5e2*ASES+i5e3*CSES+i5e4*SC+i5e5*AP

TPQ_NS~b1*OC+p1e1*FSES+p1e2*ASES+p1e3*CSES+p1e4*SC+p1e5*AP+p1i1*LFA+p1i2*LReHo+p1i3*CBRGMV+p1i4*CBLGMV+p1i5*PAT_R
TPQ_RD~b2*OC+p2e1*FSES+p2e2*ASES+p2e3*CSES+p2e4*SC+p2e5*AP+p2i1*LFA+p2i2*LReHo+p2i3*CBRGMV+p2i4*CBLGMV+p2i5*PAT_R
EC~b3*OC+p3e1*FSES+p3e2*ASES+p3e3*CSES+p3e4*SC+p3e5*AP+p3i1*LFA+p3i2*LReHo+p3i3*CBRGMV+p3i4*CBLGMV+p3i5*PAT_R
WM~b4*OC+p4e1*FSES+p4e2*ASES+p4e3*CSES+p4e4*SC+p4e5*AP+p4i1*LFA+p4i2*LReHo+p4i3*CBRGMV+p4i4*CBLGMV+p4i5*PAT_R
IFR~b5*OC+p5e1*FSES+p5e2*ASES+p5e3*CSES+p5e4*SC+p5e5*AP+p5i1*LFA+p5i2*LReHo+p5i3*CBRGMV+p5i4*CBLGMV+p5i5*PAT_R
SWL~b6*OC+p6e1*FSES+p6e2*ASES+p6e3*CSES+p6e4*SC+p6e5*AP+p6i1*LFA+p6i2*LReHo+p6i3*CBRGMV+p6i4*CBLGMV+p6i5*PAT_R
IGS_SI~b7*OC+p7e1*FSES+p7e2*ASES+p7e3*CSES+p7e4*SC+p7e5*AP+p7i1*LFA+p7i2*LReHo+p7i3*CBRGMV+p7i4*CBLGMV+p7i5*PAT_R
IRI_PT~b8*OC+p8e1*FSES+p8e2*ASES+p8e3*CSES+p8e4*SC+p8e5*AP+p8i1*LFA+p8i2*LReHo+p8i3*CBRGMV+p8i4*CBLGMV+p8i5*PAT_R
BFI_O~b9*OC+p9e1*FSES+p9e2*ASES+p9e3*CSES+p9e4*SC+p9e5*AP+p9i1*LFA+p9i2*LReHo+p9i3*CBRGMV+p9i4*CBLGMV+p9i5*PAT_R
BIS_CI~b10*OC+p10e1*FSES+p10e2*ASES+p10e3*CSES+p10e4*SC+p10e5*AP+p10i1*LFA+p10i2*LReHo+p10i3*CBRGMV+p10i4*CBLGMV+p10i5*PAT_R

GWS_FSES_LFA_TPQ_NS:=o1*i1e1*p1i1
GWS_ASES_LFA_TPQ_NS:=o2*i1e2*p1i1
GWS_CSES_LFA_TPQ_NS:=o3*i1e3*p1i1
GWS_SC_LFA_TPQ_NS:=o4*i1e4*p1i1
GWS_AP_LFA_TPQ_NS:=o5*i1e5*p1i1
GWS_FSES_LReHo_TPQ_NS:=o1*i2e1*p1i2
GWS_ASES_LReHo_TPQ_NS:=o2*i2e2*p1i2
GWS_CSES_LReHo_TPQ_NS:=o3*i2e3*p1i2
GWS_SC_LReHo_TPQ_NS:=o4*i2e4*p1i2
GWS_AP_LReHo_TPQ_NS:=o5*i2e5*p1i2
GWS_FSES_CBRGMV_TPQ_NS:=o1*i3e1*p1i3
GWS_ASES_CBRGMV_TPQ_NS:=o2*i3e2*p1i3
GWS_CSES_CBRGMV_TPQ_NS:=o3*i3e3*p1i3
GWS_SC_CBRGMV_TPQ_NS:=o4*i3e4*p1i3
GWS_AP_CBRGMV_TPQ_NS:=o5*i3e5*p1i3
GWS_FSES_CBLGMV_TPQ_NS:=o1*i4e1*p1i4
GWS_ASES_CBLGMV_TPQ_NS:=o2*i4e2*p1i4
GWS_CSES_CBLGMV_TPQ_NS:=o3*i4e3*p1i4
GWS_SC_CBLGMV_TPQ_NS:=o4*i4e4*p1i4
GWS_AP_CBLGMV_TPQ_NS:=o5*i4e5*p1i4
GWS_FSES_PAT_R_TPQ_NS:=o1*i5e1*p1i5
GWS_ASES_PAT_R_TPQ_NS:=o2*i5e2*p1i5
GWS_CSES_PAT_R_TPQ_NS:=o3*i5e3*p1i5
GWS_SC_PAT_R_TPQ_NS:=o4*i5e4*p1i5
GWS_AP_PAT_R_TPQ_NS:=o5*i5e5*p1i5
GWS_FSES_LFA_TPQ_RD:=o1*i1e1*p2i1
GWS_ASES_LFA_TPQ_RD:=o2*i1e2*p2i1
GWS_CSES_LFA_TPQ_RD:=o3*i1e3*p2i1
GWS_SC_LFA_TPQ_RD:=o4*i1e4*p2i1
GWS_AP_LFA_TPQ_RD:=o5*i1e5*p2i1
GWS_FSES_LReHo_TPQ_RD:=o1*i2e1*p2i2
GWS_ASES_LReHo_TPQ_RD:=o2*i2e2*p2i2
GWS_CSES_LReHo_TPQ_RD:=o3*i2e3*p2i2
GWS_SC_LReHo_TPQ_RD:=o4*i2e4*p2i2
GWS_AP_LReHo_TPQ_RD:=o5*i2e5*p2i2
GWS_FSES_CBRGMV_TPQ_RD:=o1*i3e1*p2i3
GWS_ASES_CBRGMV_TPQ_RD:=o2*i3e2*p2i3
GWS_CSES_CBRGMV_TPQ_RD:=o3*i3e3*p2i3
GWS_SC_CBRGMV_TPQ_RD:=o4*i3e4*p2i3
GWS_AP_CBRGMV_TPQ_RD:=o5*i3e5*p2i3
GWS_FSES_CBLGMV_TPQ_RD:=o1*i4e1*p2i4
GWS_ASES_CBLGMV_TPQ_RD:=o2*i4e2*p2i4
GWS_CSES_CBLGMV_TPQ_RD:=o3*i4e3*p2i4
GWS_SC_CBLGMV_TPQ_RD:=o4*i4e4*p2i4
GWS_AP_CBLGMV_TPQ_RD:=o5*i4e5*p2i4
GWS_FSES_PAT_R_TPQ_RD:=o1*i5e1*p2i5
GWS_ASES_PAT_R_TPQ_RD:=o2*i5e2*p2i5
GWS_CSES_PAT_R_TPQ_RD:=o3*i5e3*p2i5
GWS_SC_PAT_R_TPQ_RD:=o4*i5e4*p2i5
GWS_AP_PAT_R_TPQ_RD:=o5*i5e5*p2i5
GWS_FSES_LFA_EC:=o1*i1e1*p3i1
GWS_ASES_LFA_EC:=o2*i1e2*p3i1
GWS_CSES_LFA_EC:=o3*i1e3*p3i1
GWS_SC_LFA_EC:=o4*i1e4*p3i1
GWS_AP_LFA_EC:=o5*i1e5*p3i1
GWS_FSES_LReHo_EC:=o1*i2e1*p3i2
GWS_ASES_LReHo_EC:=o2*i2e2*p3i2
GWS_CSES_LReHo_EC:=o3*i2e3*p3i2
GWS_SC_LReHo_EC:=o4*i2e4*p3i2
GWS_AP_LReHo_EC:=o5*i2e5*p3i2
GWS_FSES_CBRGMV_EC:=o1*i3e1*p3i3
GWS_ASES_CBRGMV_EC:=o2*i3e2*p3i3
GWS_CSES_CBRGMV_EC:=o3*i3e3*p3i3
GWS_SC_CBRGMV_EC:=o4*i3e4*p3i3
GWS_AP_CBRGMV_EC:=o5*i3e5*p3i3
GWS_FSES_CBLGMV_EC:=o1*i4e1*p3i4
GWS_ASES_CBLGMV_EC:=o2*i4e2*p3i4
GWS_CSES_CBLGMV_EC:=o3*i4e3*p3i4
GWS_SC_CBLGMV_EC:=o4*i4e4*p3i4
GWS_AP_CBLGMV_EC:=o5*i4e5*p3i4
GWS_FSES_PAT_R_EC:=o1*i5e1*p3i5
GWS_ASES_PAT_R_EC:=o2*i5e2*p3i5
GWS_CSES_PAT_R_EC:=o3*i5e3*p3i5
GWS_SC_PAT_R_EC:=o4*i5e4*p3i5
GWS_AP_PAT_R_EC:=o5*i5e5*p3i5
GWS_FSES_LFA_WM:=o1*i1e1*p4i1
GWS_ASES_LFA_WM:=o2*i1e2*p4i1
GWS_CSES_LFA_WM:=o3*i1e3*p4i1
GWS_SC_LFA_WM:=o4*i1e4*p4i1
GWS_AP_LFA_WM:=o5*i1e5*p4i1
GWS_FSES_LReHo_WM:=o1*i2e1*p4i2
GWS_ASES_LReHo_WM:=o2*i2e2*p4i2
GWS_CSES_LReHo_WM:=o3*i2e3*p4i2
GWS_SC_LReHo_WM:=o4*i2e4*p4i2
GWS_AP_LReHo_WM:=o5*i2e5*p4i2
GWS_FSES_CBRGMV_WM:=o1*i3e1*p4i3
GWS_ASES_CBRGMV_WM:=o2*i3e2*p4i3
GWS_CSES_CBRGMV_WM:=o3*i3e3*p4i3
GWS_SC_CBRGMV_WM:=o4*i3e4*p4i3
GWS_AP_CBRGMV_WM:=o5*i3e5*p4i3
GWS_FSES_CBLGMV_WM:=o1*i4e1*p4i4
GWS_ASES_CBLGMV_WM:=o2*i4e2*p4i4
GWS_CSES_CBLGMV_WM:=o3*i4e3*p4i4
GWS_SC_CBLGMV_WM:=o4*i4e4*p4i4
GWS_AP_CBLGMV_WM:=o5*i4e5*p4i4
GWS_FSES_PAT_R_WM:=o1*i5e1*p4i5
GWS_ASES_PAT_R_WM:=o2*i5e2*p4i5
GWS_CSES_PAT_R_WM:=o3*i5e3*p4i5
GWS_SC_PAT_R_WM:=o4*i5e4*p4i5
GWS_AP_PAT_R_WM:=o5*i5e5*p4i5
GWS_FSES_LFA_IFR:=o1*i1e1*p5i1
GWS_ASES_LFA_IFR:=o2*i1e2*p5i1
GWS_CSES_LFA_IFR:=o3*i1e3*p5i1
GWS_SC_LFA_IFR:=o4*i1e4*p5i1
GWS_AP_LFA_IFR:=o5*i1e5*p5i1
GWS_FSES_LReHo_IFR:=o1*i2e1*p5i2
GWS_ASES_LReHo_IFR:=o2*i2e2*p5i2
GWS_CSES_LReHo_IFR:=o3*i2e3*p5i2
GWS_SC_LReHo_IFR:=o4*i2e4*p5i2
GWS_AP_LReHo_IFR:=o5*i2e5*p5i2
GWS_FSES_CBRGMV_IFR:=o1*i3e1*p5i3
GWS_ASES_CBRGMV_IFR:=o2*i3e2*p5i3
GWS_CSES_CBRGMV_IFR:=o3*i3e3*p5i3
GWS_SC_CBRGMV_IFR:=o4*i3e4*p5i3
GWS_AP_CBRGMV_IFR:=o5*i3e5*p5i3
GWS_FSES_CBLGMV_IFR:=o1*i4e1*p5i4
GWS_ASES_CBLGMV_IFR:=o2*i4e2*p5i4
GWS_CSES_CBLGMV_IFR:=o3*i4e3*p5i4
GWS_SC_CBLGMV_IFR:=o4*i4e4*p5i4
GWS_AP_CBLGMV_IFR:=o5*i4e5*p5i4
GWS_FSES_PAT_R_IFR:=o1*i5e1*p5i5
GWS_ASES_PAT_R_IFR:=o2*i5e2*p5i5
GWS_CSES_PAT_R_IFR:=o3*i5e3*p5i5
GWS_SC_PAT_R_IFR:=o4*i5e4*p5i5
GWS_AP_PAT_R_IFR:=o5*i5e5*p5i5
GWS_FSES_LFA_SWL:=o1*i1e1*p6i1
GWS_ASES_LFA_SWL:=o2*i1e2*p6i1
GWS_CSES_LFA_SWL:=o3*i1e3*p6i1
GWS_SC_LFA_SWL:=o4*i1e4*p6i1
GWS_AP_LFA_SWL:=o5*i1e5*p6i1
GWS_FSES_LReHo_SWL:=o1*i2e1*p6i2
GWS_ASES_LReHo_SWL:=o2*i2e2*p6i2
GWS_CSES_LReHo_SWL:=o3*i2e3*p6i2
GWS_SC_LReHo_SWL:=o4*i2e4*p6i2
GWS_AP_LReHo_SWL:=o5*i2e5*p6i2
GWS_FSES_CBRGMV_SWL:=o1*i3e1*p6i3
GWS_ASES_CBRGMV_SWL:=o2*i3e2*p6i3
GWS_CSES_CBRGMV_SWL:=o3*i3e3*p6i3
GWS_SC_CBRGMV_SWL:=o4*i3e4*p6i3
GWS_AP_CBRGMV_SWL:=o5*i3e5*p6i3
GWS_FSES_CBLGMV_SWL:=o1*i4e1*p6i4
GWS_ASES_CBLGMV_SWL:=o2*i4e2*p6i4
GWS_CSES_CBLGMV_SWL:=o3*i4e3*p6i4
GWS_SC_CBLGMV_SWL:=o4*i4e4*p6i4
GWS_AP_CBLGMV_SWL:=o5*i4e5*p6i4
GWS_FSES_PAT_R_SWL:=o1*i5e1*p6i5
GWS_ASES_PAT_R_SWL:=o2*i5e2*p6i5
GWS_CSES_PAT_R_SWL:=o3*i5e3*p6i5
GWS_SC_PAT_R_SWL:=o4*i5e4*p6i5
GWS_AP_PAT_R_SWL:=o5*i5e5*p6i5
GWS_FSES_LFA_IGS_SI:=o1*i1e1*p7i1
GWS_ASES_LFA_IGS_SI:=o2*i1e2*p7i1
GWS_CSES_LFA_IGS_SI:=o3*i1e3*p7i1
GWS_SC_LFA_IGS_SI:=o4*i1e4*p7i1
GWS_AP_LFA_IGS_SI:=o5*i1e5*p7i1
GWS_FSES_LReHo_IGS_SI:=o1*i2e1*p7i2
GWS_ASES_LReHo_IGS_SI:=o2*i2e2*p7i2
GWS_CSES_LReHo_IGS_SI:=o3*i2e3*p7i2
GWS_SC_LReHo_IGS_SI:=o4*i2e4*p7i2
GWS_AP_LReHo_IGS_SI:=o5*i2e5*p7i2
GWS_FSES_CBRGMV_IGS_SI:=o1*i3e1*p7i3
GWS_ASES_CBRGMV_IGS_SI:=o2*i3e2*p7i3
GWS_CSES_CBRGMV_IGS_SI:=o3*i3e3*p7i3
GWS_SC_CBRGMV_IGS_SI:=o4*i3e4*p7i3
GWS_AP_CBRGMV_IGS_SI:=o5*i3e5*p7i3
GWS_FSES_CBLGMV_IGS_SI:=o1*i4e1*p7i4
GWS_ASES_CBLGMV_IGS_SI:=o2*i4e2*p7i4
GWS_CSES_CBLGMV_IGS_SI:=o3*i4e3*p7i4
GWS_SC_CBLGMV_IGS_SI:=o4*i4e4*p7i4
GWS_AP_CBLGMV_IGS_SI:=o5*i4e5*p7i4
GWS_FSES_PAT_R_IGS_SI:=o1*i5e1*p7i5
GWS_ASES_PAT_R_IGS_SI:=o2*i5e2*p7i5
GWS_CSES_PAT_R_IGS_SI:=o3*i5e3*p7i5
GWS_SC_PAT_R_IGS_SI:=o4*i5e4*p7i5
GWS_AP_PAT_R_IGS_SI:=o5*i5e5*p7i5
GWS_FSES_LFA_IRI_PT:=o1*i1e1*p8i1
GWS_ASES_LFA_IRI_PT:=o2*i1e2*p8i1
GWS_CSES_LFA_IRI_PT:=o3*i1e3*p8i1
GWS_SC_LFA_IRI_PT:=o4*i1e4*p8i1
GWS_AP_LFA_IRI_PT:=o5*i1e5*p8i1
GWS_FSES_LReHo_IRI_PT:=o1*i2e1*p8i2
GWS_ASES_LReHo_IRI_PT:=o2*i2e2*p8i2
GWS_CSES_LReHo_IRI_PT:=o3*i2e3*p8i2
GWS_SC_LReHo_IRI_PT:=o4*i2e4*p8i2
GWS_AP_LReHo_IRI_PT:=o5*i2e5*p8i2
GWS_FSES_CBRGMV_IRI_PT:=o1*i3e1*p8i3
GWS_ASES_CBRGMV_IRI_PT:=o2*i3e2*p8i3
GWS_CSES_CBRGMV_IRI_PT:=o3*i3e3*p8i3
GWS_SC_CBRGMV_IRI_PT:=o4*i3e4*p8i3
GWS_AP_CBRGMV_IRI_PT:=o5*i3e5*p8i3
GWS_FSES_CBLGMV_IRI_PT:=o1*i4e1*p8i4
GWS_ASES_CBLGMV_IRI_PT:=o2*i4e2*p8i4
GWS_CSES_CBLGMV_IRI_PT:=o3*i4e3*p8i4
GWS_SC_CBLGMV_IRI_PT:=o4*i4e4*p8i4
GWS_AP_CBLGMV_IRI_PT:=o5*i4e5*p8i4
GWS_FSES_PAT_R_IRI_PT:=o1*i5e1*p8i5
GWS_ASES_PAT_R_IRI_PT:=o2*i5e2*p8i5
GWS_CSES_PAT_R_IRI_PT:=o3*i5e3*p8i5
GWS_SC_PAT_R_IRI_PT:=o4*i5e4*p8i5
GWS_AP_PAT_R_IRI_PT:=o5*i5e5*p8i5
GWS_FSES_LFA_BFI_O:=o1*i1e1*p9i1
GWS_ASES_LFA_BFI_O:=o2*i1e2*p9i1
GWS_CSES_LFA_BFI_O:=o3*i1e3*p9i1
GWS_SC_LFA_BFI_O:=o4*i1e4*p9i1
GWS_AP_LFA_BFI_O:=o5*i1e5*p9i1
GWS_FSES_LReHo_BFI_O:=o1*i2e1*p9i2
GWS_ASES_LReHo_BFI_O:=o2*i2e2*p9i2
GWS_CSES_LReHo_BFI_O:=o3*i2e3*p9i2
GWS_SC_LReHo_BFI_O:=o4*i2e4*p9i2
GWS_AP_LReHo_BFI_O:=o5*i2e5*p9i2
GWS_FSES_CBRGMV_BFI_O:=o1*i3e1*p9i3
GWS_ASES_CBRGMV_BFI_O:=o2*i3e2*p9i3
GWS_CSES_CBRGMV_BFI_O:=o3*i3e3*p9i3
GWS_SC_CBRGMV_BFI_O:=o4*i3e4*p9i3
GWS_AP_CBRGMV_BFI_O:=o5*i3e5*p9i3
GWS_FSES_CBLGMV_BFI_O:=o1*i4e1*p9i4
GWS_ASES_CBLGMV_BFI_O:=o2*i4e2*p9i4
GWS_CSES_CBLGMV_BFI_O:=o3*i4e3*p9i4
GWS_SC_CBLGMV_BFI_O:=o4*i4e4*p9i4
GWS_AP_CBLGMV_BFI_O:=o5*i4e5*p9i4
GWS_FSES_PAT_R_BFI_O:=o1*i5e1*p9i5
GWS_ASES_PAT_R_BFI_O:=o2*i5e2*p9i5
GWS_CSES_PAT_R_BFI_O:=o3*i5e3*p9i5
GWS_SC_PAT_R_BFI_O:=o4*i5e4*p9i5
GWS_AP_PAT_R_BFI_O:=o5*i5e5*p9i5
GWS_FSES_LFA_BIS_CI:=o1*i1e1*p10i1
GWS_ASES_LFA_BIS_CI:=o2*i1e2*p10i1
GWS_CSES_LFA_BIS_CI:=o3*i1e3*p10i1
GWS_SC_LFA_BIS_CI:=o4*i1e4*p10i1
GWS_AP_LFA_BIS_CI:=o5*i1e5*p10i1
GWS_FSES_LReHo_BIS_CI:=o1*i2e1*p10i2
GWS_ASES_LReHo_BIS_CI:=o2*i2e2*p10i2
GWS_CSES_LReHo_BIS_CI:=o3*i2e3*p10i2
GWS_SC_LReHo_BIS_CI:=o4*i2e4*p10i2
GWS_AP_LReHo_BIS_CI:=o5*i2e5*p10i2
GWS_FSES_CBRGMV_BIS_CI:=o1*i3e1*p10i3
GWS_ASES_CBRGMV_BIS_CI:=o2*i3e2*p10i3
GWS_CSES_CBRGMV_BIS_CI:=o3*i3e3*p10i3
GWS_SC_CBRGMV_BIS_CI:=o4*i3e4*p10i3
GWS_AP_CBRGMV_BIS_CI:=o5*i3e5*p10i3
GWS_FSES_CBLGMV_BIS_CI:=o1*i4e1*p10i4
GWS_ASES_CBLGMV_BIS_CI:=o2*i4e2*p10i4
GWS_CSES_CBLGMV_BIS_CI:=o3*i4e3*p10i4
GWS_SC_CBLGMV_BIS_CI:=o4*i4e4*p10i4
GWS_AP_CBLGMV_BIS_CI:=o5*i4e5*p10i4
GWS_FSES_PAT_R_BIS_CI:=o1*i5e1*p10i5
GWS_ASES_PAT_R_BIS_CI:=o2*i5e2*p10i5
GWS_CSES_PAT_R_BIS_CI:=o3*i5e3*p10i5
GWS_SC_PAT_R_BIS_CI:=o4*i5e4*p10i5
GWS_AP_PAT_R_BIS_CI:=o5*i5e5*p10i5

GWS_FSES_TPQ_NS:=o1*p1e1
GWS_ASES_TPQ_NS:=o2*p1e2
GWS_CSES_TPQ_NS:=o3*p1e3
GWS_SC_TPQ_NS:=o4*p1e4
GWS_AP_TPQ_NS:=o5*p1e5
GWS_FSES_TPQ_RD:=o1*p2e1
GWS_ASES_TPQ_RD:=o2*p2e2
GWS_CSES_TPQ_RD:=o3*p2e3
GWS_SC_TPQ_RD:=o4*p2e4
GWS_AP_TPQ_RD:=o5*p2e5
GWS_FSES_EC:=o1*p3e1
GWS_ASES_EC:=o2*p3e2
GWS_CSES_EC:=o3*p3e3
GWS_SC_EC:=o4*p3e4
GWS_AP_EC:=o5*p3e5
GWS_FSES_WM:=o1*p4e1
GWS_ASES_WM:=o2*p4e2
GWS_CSES_WM:=o3*p4e3
GWS_SC_WM:=o4*p4e4
GWS_AP_WM:=o5*p4e5
GWS_FSES_IFR:=o1*p5e1
GWS_ASES_IFR:=o2*p5e2
GWS_CSES_IFR:=o3*p5e3
GWS_SC_IFR:=o4*p5e4
GWS_AP_IFR:=o5*p5e5
GWS_FSES_SWL:=o1*p6e1
GWS_ASES_SWL:=o2*p6e2
GWS_CSES_SWL:=o3*p6e3
GWS_SC_SWL:=o4*p6e4
GWS_AP_SWL:=o5*p6e5
GWS_FSES_IGS_SI:=o1*p7e1
GWS_ASES_IGS_SI:=o2*p7e2
GWS_CSES_IGS_SI:=o3*p7e3
GWS_SC_IGS_SI:=o4*p7e4
GWS_AP_IGS_SI:=o5*p7e5
GWS_FSES_IRI_PT:=o1*p8e1
GWS_ASES_IRI_PT:=o2*p8e2
GWS_CSES_IRI_PT:=o3*p8e3
GWS_SC_IRI_PT:=o4*p8e4
GWS_AP_IRI_PT:=o5*p8e5
GWS_FSES_BFI_O:=o1*p9e1
GWS_ASES_BFI_O:=o2*p9e2
GWS_CSES_BFI_O:=o3*p9e3
GWS_SC_BFI_O:=o4*p9e4
GWS_AP_BFI_O:=o5*p9e5
GWS_FSES_BIS_CI:=o1*p10e1
GWS_ASES_BIS_CI:=o2*p10e2
GWS_CSES_BIS_CI:=o3*p10e3
GWS_SC_BIS_CI:=o4*p10e4
GWS_AP_BIS_CI:=o5*p10e5

GWS_LFA_TPQ_NS:=c1*p1i1
GWS_LReHo_TPQ_NS:=c2*p1i2
GWS_CBRGMV_TPQ_NS:=c3*p1i3
GWS_CBLGMV_TPQ_NS:=c4*p1i4
GWS_PAT_R_TPQ_NS:=c5*p1i5
GWS_LFA_TPQ_RD:=c1*p2i1
GWS_LReHo_TPQ_RD:=c2*p2i2
GWS_CBRGMV_TPQ_RD:=c3*p2i3
GWS_CBLGMV_TPQ_RD:=c4*p2i4
GWS_PAT_R_TPQ_RD:=c5*p2i5
GWS_LFA_EC:=c1*p3i1
GWS_LReHo_EC:=c2*p3i2
GWS_CBRGMV_EC:=c3*p3i3
GWS_CBLGMV_EC:=c4*p3i4
GWS_PAT_R_EC:=c5*p3i5
GWS_LFA_WM:=c1*p4i1
GWS_LReHo_WM:=c2*p4i2
GWS_CBRGMV_WM:=c3*p4i3
GWS_CBLGMV_WM:=c4*p4i4
GWS_PAT_R_WM:=c5*p4i5
GWS_LFA_IFR:=c1*p5i1
GWS_LReHo_IFR:=c2*p5i2
GWS_CBRGMV_IFR:=c3*p5i3
GWS_CBLGMV_IFR:=c4*p5i4
GWS_PAT_R_IFR:=c5*p5i5
GWS_LFA_SWL:=c1*p6i1
GWS_LReHo_SWL:=c2*p6i2
GWS_CBRGMV_SWL:=c3*p6i3
GWS_CBLGMV_SWL:=c4*p6i4
GWS_PAT_R_SWL:=c5*p6i5
GWS_LFA_IGS_SI:=c1*p7i1
GWS_LReHo_IGS_SI:=c2*p7i2
GWS_CBRGMV_IGS_SI:=c3*p7i3
GWS_CBLGMV_IGS_SI:=c4*p7i4
GWS_PAT_R_IGS_SI:=c5*p7i5
GWS_LFA_IRI_PT:=c1*p8i1
GWS_LReHo_IRI_PT:=c2*p8i2
GWS_CBRGMV_IRI_PT:=c3*p8i3
GWS_CBLGMV_IRI_PT:=c4*p8i4
GWS_PAT_R_IRI_PT:=c5*p8i5
GWS_LFA_BFI_O:=c1*p9i1
GWS_LReHo_BFI_O:=c2*p9i2
GWS_CBRGMV_BFI_O:=c3*p9i3
GWS_CBLGMV_BFI_O:=c4*p9i4
GWS_PAT_R_BFI_O:=c5*p9i5
GWS_LFA_BIS_CI:=c1*p10i1
GWS_LReHo_BIS_CI:=c2*p10i2
GWS_CBRGMV_BIS_CI:=c3*p10i3
GWS_CBLGMV_BIS_CI:=c4*p10i4
GWS_PAT_R_BIS_CI:=c5*p10i5

GWS_FSES_LFA:=o1*i1e1
GWS_ASES_LFA:=o2*i1e2
GWS_CSES_LFA:=o3*i1e3
GWS_SC_LFA:=o4*i1e4
GWS_AP_LFA:=o5*i1e5
GWS_FSES_LReHo:=o1*i2e1
GWS_ASES_LReHo:=o2*i2e2
GWS_CSES_LReHo:=o3*i2e3
GWS_SC_LReHo:=o4*i2e4
GWS_AP_LReHo:=o5*i2e5
GWS_FSES_CBRGMV:=o1*i3e1
GWS_ASES_CBRGMV:=o2*i3e2
GWS_CSES_CBRGMV:=o3*i3e3
GWS_SC_CBRGMV:=o4*i3e4
GWS_AP_CBRGMV:=o5*i3e5
GWS_FSES_CBLGMV:=o1*i4e1
GWS_ASES_CBLGMV:=o2*i4e2
GWS_CSES_CBLGMV:=o3*i4e3
GWS_SC_CBLGMV:=o4*i4e4
GWS_AP_CBLGMV:=o5*i4e5
GWS_FSES_PAT_R:=o1*i5e1
GWS_ASES_PAT_R:=o2*i5e2
GWS_CSES_PAT_R:=o3*i5e3
GWS_SC_PAT_R:=o4*i5e4
GWS_AP_PAT_R:=o5*i5e5

TotalIndirect_TPQ_NS:=c1*p1i1+c2*p1i2+c3*p1i3+c4*p1i4+c5*p1i5+o1*i1e1*p1i1+o2*i1e2*p1i1+o3*i1e3*p1i1+o4*i1e4*p1i1+o5*i1e5*p1i1+o1*i2e1*p1i2+o2*i2e2*p1i2+o3*i2e3*p1i2+o4*i2e4*p1i2+o5*i2e5*p1i2+o1*i3e1*p1i3+o2*i3e2*p1i3+o3*i3e3*p1i3+o4*i3e4*p1i3+o5*i3e5*p1i3+o1*i4e1*p1i4+o2*i4e2*p1i4+o3*i4e3*p1i4+o4*i4e4*p1i4+o5*i4e5*p1i4+o1*i5e1*p1i5+o2*i5e2*p1i5+o3*i5e3*p1i5+o4*i5e4*p1i5+o5*i5e5*p1i5+o1*p1e1+o2*p1e2+o3*p1e3+o4*p1e4+o5*p1e5
TotalIndirect_TPQ_RD:=c1*p2i1+c2*p2i2+c3*p2i3+c4*p2i4+c5*p2i5+o1*i1e1*p2i1+o2*i1e2*p2i1+o3*i1e3*p2i1+o4*i1e4*p2i1+o5*i1e5*p2i1+o1*i2e1*p2i2+o2*i2e2*p2i2+o3*i2e3*p2i2+o4*i2e4*p2i2+o5*i2e5*p2i2+o1*i3e1*p2i3+o2*i3e2*p2i3+o3*i3e3*p2i3+o4*i3e4*p2i3+o5*i3e5*p2i3+o1*i4e1*p2i4+o2*i4e2*p2i4+o3*i4e3*p2i4+o4*i4e4*p2i4+o5*i4e5*p2i4+o1*i5e1*p2i5+o2*i5e2*p2i5+o3*i5e3*p2i5+o4*i5e4*p2i5+o5*i5e5*p2i5+o1*p2e1+o2*p2e2+o3*p2e3+o4*p2e4+o5*p2e5
TotalIndirect_EC:=c1*p3i1+c2*p3i2+c3*p3i3+c4*p3i4+c5*p3i5+o1*i1e1*p3i1+o2*i1e2*p3i1+o3*i1e3*p3i1+o4*i1e4*p3i1+o5*i1e5*p3i1+o1*i2e1*p3i2+o2*i2e2*p3i2+o3*i2e3*p3i2+o4*i2e4*p3i2+o5*i2e5*p3i2+o1*i3e1*p3i3+o2*i3e2*p3i3+o3*i3e3*p3i3+o4*i3e4*p3i3+o5*i3e5*p3i3+o1*i4e1*p3i4+o2*i4e2*p3i4+o3*i4e3*p3i4+o4*i4e4*p3i4+o5*i4e5*p3i4+o1*i5e1*p3i5+o2*i5e2*p3i5+o3*i5e3*p3i5+o4*i5e4*p3i5+o5*i5e5*p3i5+o1*p3e1+o2*p3e2+o3*p3e3+o4*p3e4+o5*p3e5
TotalIndirect_WM:=c1*p4i1+c2*p4i2+c3*p4i3+c4*p4i4+c5*p4i5+o1*i1e1*p4i1+o2*i1e2*p4i1+o3*i1e3*p4i1+o4*i1e4*p4i1+o5*i1e5*p4i1+o1*i2e1*p4i2+o2*i2e2*p4i2+o3*i2e3*p4i2+o4*i2e4*p4i2+o5*i2e5*p4i2+o1*i3e1*p4i3+o2*i3e2*p4i3+o3*i3e3*p4i3+o4*i3e4*p4i3+o5*i3e5*p4i3+o1*i4e1*p4i4+o2*i4e2*p4i4+o3*i4e3*p4i4+o4*i4e4*p4i4+o5*i4e5*p4i4+o1*i5e1*p4i5+o2*i5e2*p4i5+o3*i5e3*p4i5+o4*i5e4*p4i5+o5*i5e5*p4i5+o1*p4e1+o2*p4e2+o3*p4e3+o4*p4e4+o5*p4e5
TotalIndirect_IFR:=c1*p5i1+c2*p5i2+c3*p5i3+c4*p5i4+c5*p5i5+o1*i1e1*p5i1+o2*i1e2*p5i1+o3*i1e3*p5i1+o4*i1e4*p5i1+o5*i1e5*p5i1+o1*i2e1*p5i2+o2*i2e2*p5i2+o3*i2e3*p5i2+o4*i2e4*p5i2+o5*i2e5*p5i2+o1*i3e1*p5i3+o2*i3e2*p5i3+o3*i3e3*p5i3+o4*i3e4*p5i3+o5*i3e5*p5i3+o1*i4e1*p5i4+o2*i4e2*p5i4+o3*i4e3*p5i4+o4*i4e4*p5i4+o5*i4e5*p5i4+o1*i5e1*p5i5+o2*i5e2*p5i5+o3*i5e3*p5i5+o4*i5e4*p5i5+o5*i5e5*p5i5+o1*p5e1+o2*p5e2+o3*p5e3+o4*p5e4+o5*p5e5
TotalIndirect_SWL:=c1*p6i1+c2*p6i2+c3*p6i3+c4*p6i4+c5*p6i5+o1*i1e1*p6i1+o2*i1e2*p6i1+o3*i1e3*p6i1+o4*i1e4*p6i1+o5*i1e5*p6i1+o1*i2e1*p6i2+o2*i2e2*p6i2+o3*i2e3*p6i2+o4*i2e4*p6i2+o5*i2e5*p6i2+o1*i3e1*p6i3+o2*i3e2*p6i3+o3*i3e3*p6i3+o4*i3e4*p6i3+o5*i3e5*p6i3+o1*i4e1*p6i4+o2*i4e2*p6i4+o3*i4e3*p6i4+o4*i4e4*p6i4+o5*i4e5*p6i4+o1*i5e1*p6i5+o2*i5e2*p6i5+o3*i5e3*p6i5+o4*i5e4*p6i5+o5*i5e5*p6i5+o1*p6e1+o2*p6e2+o3*p6e3+o4*p6e4+o5*p6e5
TotalIndirect_IGS_SI:=c1*p7i1+c2*p7i2+c3*p7i3+c4*p7i4+c5*p7i5+o1*i1e1*p7i1+o2*i1e2*p7i1+o3*i1e3*p7i1+o4*i1e4*p7i1+o5*i1e5*p7i1+o1*i2e1*p7i2+o2*i2e2*p7i2+o3*i2e3*p7i2+o4*i2e4*p7i2+o5*i2e5*p7i2+o1*i3e1*p7i3+o2*i3e2*p7i3+o3*i3e3*p7i3+o4*i3e4*p7i3+o5*i3e5*p7i3+o1*i4e1*p7i4+o2*i4e2*p7i4+o3*i4e3*p7i4+o4*i4e4*p7i4+o5*i4e5*p7i4+o1*i5e1*p7i5+o2*i5e2*p7i5+o3*i5e3*p7i5+o4*i5e4*p7i5+o5*i5e5*p7i5+o1*p7e1+o2*p7e2+o3*p7e3+o4*p7e4+o5*p7e5
TotalIndirect_IRI_PT:=c1*p8i1+c2*p8i2+c3*p8i3+c4*p8i4+c5*p8i5+o1*i1e1*p8i1+o2*i1e2*p8i1+o3*i1e3*p8i1+o4*i1e4*p8i1+o5*i1e5*p8i1+o1*i2e1*p8i2+o2*i2e2*p8i2+o3*i2e3*p8i2+o4*i2e4*p8i2+o5*i2e5*p8i2+o1*i3e1*p8i3+o2*i3e2*p8i3+o3*i3e3*p8i3+o4*i3e4*p8i3+o5*i3e5*p8i3+o1*i4e1*p8i4+o2*i4e2*p8i4+o3*i4e3*p8i4+o4*i4e4*p8i4+o5*i4e5*p8i4+o1*i5e1*p8i5+o2*i5e2*p8i5+o3*i5e3*p8i5+o4*i5e4*p8i5+o5*i5e5*p8i5+o1*p8e1+o2*p8e2+o3*p8e3+o4*p8e4+o5*p8e5
TotalIndirect_BFI_O:=c1*p9i1+c2*p9i2+c3*p9i3+c4*p9i4+c5*p9i5+o1*i1e1*p9i1+o2*i1e2*p9i1+o3*i1e3*p9i1+o4*i1e4*p9i1+o5*i1e5*p9i1+o1*i2e1*p9i2+o2*i2e2*p9i2+o3*i2e3*p9i2+o4*i2e4*p9i2+o5*i2e5*p9i2+o1*i3e1*p9i3+o2*i3e2*p9i3+o3*i3e3*p9i3+o4*i3e4*p9i3+o5*i3e5*p9i3+o1*i4e1*p9i4+o2*i4e2*p9i4+o3*i4e3*p9i4+o4*i4e4*p9i4+o5*i4e5*p9i4+o1*i5e1*p9i5+o2*i5e2*p9i5+o3*i5e3*p9i5+o4*i5e4*p9i5+o5*i5e5*p9i5+o1*p9e1+o2*p9e2+o3*p9e3+o4*p9e4+o5*p9e5
TotalIndirect_BIS_CI:=c1*p10i1+c2*p10i2+c3*p10i3+c4*p10i4+c5*p10i5+o1*i1e1*p10i1+o2*i1e2*p10i1+o3*i1e3*p10i1+o4*i1e4*p10i1+o5*i1e5*p10i1+o1*i2e1*p10i2+o2*i2e2*p10i2+o3*i2e3*p10i2+o4*i2e4*p10i2+o5*i2e5*p10i2+o1*i3e1*p10i3+o2*i3e2*p10i3+o3*i3e3*p10i3+o4*i3e4*p10i3+o5*i3e5*p10i3+o1*i4e1*p10i4+o2*i4e2*p10i4+o3*i4e3*p10i4+o4*i4e4*p10i4+o5*i4e5*p10i4+o1*i5e1*p10i5+o2*i5e2*p10i5+o3*i5e3*p10i5+o4*i5e4*p10i5+o5*i5e5*p10i5+o1*p10e1+o2*p10e2+o3*p10e3+o4*p10e4+o5*p10e5

Total_TPQ_NS:=TotalIndirect_TPQ_NS+b1
Total_TPQ_RD:=TotalIndirect_TPQ_RD+b2
Total_EC:=TotalIndirect_EC+b3
Total_WM:=TotalIndirect_WM+b4
Total_IFR:=TotalIndirect_IFR+b5
Total_SWL:=TotalIndirect_SWL+b6
Total_IGS_SI:=TotalIndirect_IGS_SI+b7
Total_IRI_PT:=TotalIndirect_IRI_PT+b8
Total_BFI_O:=TotalIndirect_BFI_O+b9
Total_BIS_CI:=TotalIndirect_BIS_CI+b10

TotalIndIMG_LFA:=o1*i1e1+o2*i1e2+o3*i1e3+o4*i1e4+o5*i1e5
TotalIndIMG_LReHo:=o1*i2e1+o2*i2e2+o3*i2e3+o4*i2e4+o5*i2e5
TotalIndIMG_CBRGMV:=o1*i3e1+o2*i3e2+o3*i3e3+o4*i3e4+o5*i3e5
TotalIndIMG_CBLGMV:=o1*i4e1+o2*i4e2+o3*i4e3+o4*i4e4+o5*i4e5
TotalIndIMG_PAT_R:=o1*i5e1+o2*i5e2+o3*i5e3+o4*i5e4+o5*i5e5

Total_LFA:=TotalIndIMG_LFA+c1
Total_LReHo:=TotalIndIMG_LReHo+c2
Total_CBRGMV:=TotalIndIMG_CBRGMV+c3
Total_CBLGMV:=TotalIndIMG_CBLGMV+c4
Total_PAT_R:=TotalIndIMG_PAT_R+c5

Direct_LFA:=c1
Direct_LReHo:=c2
Direct_CBRGMV:=c3
Direct_CBLGMV:=c4
Direct_PAT_R:=c5
Direct_TPQ_NS:=b1
Direct_TPQ_RD:=b2
Direct_EC:=b3
Direct_WM:=b4
Direct_IFR:=b5
Direct_SWL:=b6
Direct_IGS_SI:=b7
Direct_IRI_PT:=b8
Direct_BFI_O:=b9
Direct_BIS_CI:=b10


DRTT_L ~~     DRTT_R
DRTT_L ~~      RST_L
DRTT_R ~~      RST_L
RST_L ~~      CBT_L
CST_R ~~ DRTT_R
PE ~~     PO
PO ~~     MO
ME ~~     MO




FSES~~ 1*FSES
ASES~~ 1*ASES
CSES~~ 1*CSES
SC~~ 1*SC
AP~~ 1*AP

LFA~~ 1*LFA
LReHo~~ 1*LReHo
CBRGMV~~ 1*CBRGMV
CBLGMV~~ 1*CBLGMV
PAT_R~~ 1*PAT_R

TPQ_NS~~ 1*TPQ_NS
TPQ_RD~~ 1*TPQ_RD
EC~~ 1*EC
WM~~ 1*WM
IFR~~ 1*IFR
SWL~~ 1*SWL
IGS_SI~~ 1*IGS_SI
IRI_PT~~ 1*IRI_PT
BFI_O~~ 1*BFI_O
BIS_CI~~ 1*BIS_CI
OC~~ 1*OC
'
fit.mod <-lavaan::sem(mod,data = sigenvcogimg,std.ov=T,std.lv=T)
fitmeasures(fit.mod, c( "cfi",'tli', "rmsea",'srmr','gfi',"chisq", "df", "pvalue"))
esti<-parameterestimates(fit.mod,standardized = TRUE,rsquare=T)
