/* This is the header for the segy trace header.  Reading bytes
  directly into this header will allow access to all of the fields.
*/
typedef struct SegyHead {
  long lineSeq, reelSeq; /* Sequence numbers within line and reel, resp. */
  long event_number; /* Original field record number */
  long channel_number; /* Trace number within the original field record.*/
  long energySourcePt; /* Energy source point number */
  long cdpEns; /* CDP ensemble number */
  long traceInEnsemble; /* Trace number within CDP ensemble */
  short traceID; /* Trace identification code:
	1 = seismic data	4 = time break	7 = timing
	2 = dead		5 = uphole	8 = water break
	3 = dummy		6 = sweep	9..., optional use */
  short vertSum, horSum;
  short dataUse; /* 1 = production, 2 = test */
  long sourceToRecDist;
  long recElevation;
  long sourceSurfaceElevation;
  long sourceDepth;
  long datumElevRec, datumElemSource;
  long recWaterDepth, sourceWaterDepth;
  short elevationScale;
  short coordScale;
  long sourceLongOrX, sourceLatOrY;
  long recLongOrX, recLatOrY;
  short coordUnits;
  short weatheringVelocity;
  short subWeatheringVelocity;
  short sourceUpholeTime, recUpholeTime;
  short sourceStaticCor, recStaticCor;
  short totalStatic;
  short lagTimeA, lagTimeB;
  short delay;
  short muteStart, muteEnd;
  short sampleLength; /* Number of samples in this trace */
  short deltaSample; /* Sampling interval in microseconds. */
  short gainType; /* 1 = fixed, 2 = binary, 3 = floating, 4... opt.*/
  short gainConst, initialGain;
  short correlated; /* 1 = no, 2 = yes */
  short sweepStart, sweepEnd;
  short sweepLength; /* in milliseconds */
  short sweepType; /* 1 = linear, 2 = parabolic, 3 = exponential, 4... opt. */
  short sweepTaperAtStart, sweepTaperAtEnd;
  short taperType;
  short aliasFreq, aliasSlope;
  short notchFreq, notchSlope;
  short lowCutFreq, hiCutFreq;
  short lowCutSlope, hiCutSlope;
  short year, day, hour, minute, second;
  short timeBasisCode;
  short traceWeightingFactor;
  short phoneRollPos1, phoneFirstTrace, phoneLastTrace;
  short gapSize;
  short taperOvertravel;
  short extrash[10];
  long  samp_rate;
  short data_form, m_secs;
  short trigyear,trigday,trighour,trigminute,trigsecond,trigmills;
  float scale_fac;
  short inst_no;
  short not_to_be_used;
  long	num_samps;
  char extra[8];
} SegyHead; /* end of segy trace header */

typedef struct SegyReel{
        long            kjob, kline, kreel;
        short           kntr, knaux, sr, kfldsr, knsamp, 
				kfsamp, dsfc, kmfold, ksort;
	unsigned char unused1[24];
	short int kunits;
	unsigned char unused2[344];
} SegyReel;
