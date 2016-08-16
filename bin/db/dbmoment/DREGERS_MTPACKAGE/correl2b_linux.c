#include"tdmt_invb.h"
void c_correlate();
float *vec();

int correlate(ss,gg,i,np2)
    int i, np2;
    struct DATA   *ss;
    struct GREEN  *gg;
    {
    int j, l, Zcor[20], zvalue;
    float cormax[20], maximum=0.0;
    float *data1, *data2, *ans;


	data1=vec(1,np2);
	data2=vec(1,np2);
	ans  =vec(1,2*np2);

    for(j=0; j < 20; j++)  /*Initialize cormax for each component*/
	 {
	 Zcor[j]  =0;
         cormax[j]=0.0;
	 }

    for(j=0; j < np2; j++) /*Load Tangential Data*/
      if(j >= (ss[i].np))
        data1[j+1]=0.0;
      else
        data1[j+1]=ss[i].t[j];


    for(j=0; j < np2; j++) /*Load TSS*/
      if(j >= (gg[i].np))
        data2[j+1]=0.0;
      else
        data2[j+1]=gg[i].u1[j];

    c_correlate(data1,data2,np2,ans);

      for(j=0; j < np2/2; j++)
           if(cormax[0] < ABS(ans[j+1]))
	     {
	     cormax[0] = ABS(ans[j+1]);
	     Zcor[0] = j;
	     }
           if(maximum < cormax[0])
	     {
	     zvalue=Zcor[0];
	     maximum=cormax[0];
	     }

    for(j=0; j < np2; j++) /*Load TDS*/
      if(j >= (gg[i].np))
        data2[j+1]=0.0;
      else
        data2[j+1]=gg[i].u2[j];

    c_correlate(data1,data2,np2,ans);

      for(j=0; j < np2/2; j++)
           if(cormax[1] < ABS(ans[j+1]))
	     {
	     cormax[1] = ABS(ans[j+1]);
	     Zcor[1] = j;
	     }
    if(maximum < cormax[1])
	     {
	     zvalue=Zcor[1];
	     maximum=cormax[1];
	     }

    for(j=0; j < np2; j++) /*Load Radial Data*/
      if(j >= (ss[i].np))
        data1[j+1]=0.0;
      else
        data1[j+1]=ss[i].r[j];

    for(j=0; j < np2; j++) /*Load RSS*/
      if(j >= (gg[i].np))
        data2[j+1]=0.0;
      else
        data2[j+1]=gg[i].u3[j];

    c_correlate(data1,data2,np2,ans);

      for(j=0; j < np2/2; j++)
           if(cormax[2] < ABS(ans[j+1]))
	     {
	     cormax[2] = ABS(ans[j+1]);
	     Zcor[2] = j;
	     }
    if(maximum < cormax[2])
	     {
	     zvalue=Zcor[2];
	     maximum=cormax[2];
	     }

    for(j=0; j < np2; j++) /*Load RDS*/
      if(j >= (gg[i].np))
        data2[j+1]=0.0;
      else
        data2[j+1]=gg[i].u4[j];

    c_correlate(data1,data2,np2,ans);

      for(j=0; j < np2/2; j++)
           if(cormax[3] < ABS(ans[j+1]))
	     {
	     cormax[3] = ABS(ans[j+1]);
	     Zcor[3] = j;
	     }
    if(maximum < cormax[3])
	     {
	     zvalue=Zcor[3];
	     maximum=cormax[3];
	     }

    for(j=0; j < np2; j++) /*Load RDD*/
      if(j >= (gg[i].np))
        data2[j+1]=0.0;
      else
        data2[j+1]=gg[i].u5[j];

    c_correlate(data1,data2,np2,ans);

      for(j=0; j < np2/2; j++)
           if(cormax[4] < ABS(ans[j+1]))
	     {
	     cormax[4] = ABS(ans[j+1]);
	     Zcor[4] = j;
	     }
    if(maximum < cormax[4])
	     {
	     zvalue=Zcor[4];
	     maximum=cormax[4];
	     }

    for(j=0; j < np2; j++) /*Load Vertical Data*/
      if(j >= (ss[i].np))
        data1[j+1]=0.0;
      else
        data1[j+1]=ss[i].z[j];

    for(j=0; j < np2; j++) /*Load ZSS*/
      if(j >= (gg[i].np))
        data2[j+1]=0.0;
      else
        data2[j+1]=gg[i].u6[j];

    c_correlate(data1,data2,np2,ans);

      for(j=0; j < np2/2; j++)
           if(cormax[5] < ABS(ans[j+1]))
	     {
	     cormax[5] = ABS(ans[j+1]);
	     Zcor[5] = j;
	     }
    if(maximum < cormax[5])
	     {
	     zvalue=Zcor[5];
	     maximum=cormax[5];
	     }

    for(j=0; j < np2; j++) /*Load ZDS*/
      if(j >= (gg[i].np))
        data2[j+1]=0.0;
      else
        data2[j+1]=gg[i].u7[j];

    c_correlate(data1,data2,np2,ans);

      for(j=0; j < np2/2; j++)
           if(cormax[6] < ABS(ans[j+1]))
	     {
	     cormax[6] = ABS(ans[j+1]);
	     Zcor[6] = j;
	     }
    if(maximum < cormax[6])
	     {
	     zvalue=Zcor[6];
	     maximum=cormax[6];
	     }

    for(j=0; j < np2; j++) /*Load ZDD*/
      if(j >= (gg[i].np))
        data2[j+1]=0.0;
      else
        data2[j+1]=gg[i].u8[j];

    c_correlate(data1,data2,np2,ans);

      for(j=0; j < np2/2; j++)
           if(cormax[7] < ABS(ans[j+1]))
	     {
	     cormax[7] = ABS(ans[j+1]);
	     Zcor[7] = j;
	     }
    if(maximum < cormax[7])
	     {
	     zvalue=Zcor[7];
	     maximum=cormax[7];
	     }


f_vec(data1,1,np2);
f_vec(data2,1,np2);
f_vec(ans,1,2*np2);

return(zvalue);

}/*END CORRELATE*/
