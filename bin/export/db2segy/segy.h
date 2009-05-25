#include <stdint.h>
/* This is the header for the segy trace header.  Reading bytes
  directly into this header will allow access to all of the fields.
*/
typedef struct SegyHead {
  int32_t lineSeq, reelSeq; /* Sequence numbers within line and reel, resp. */
  int32_t event_number; /* Original field record number */
  int32_t channel_number; /* Trace number within the original field record.*/
  int32_t energySourcePt; /* Energy source point number */
  int32_t cdpEns; /* CDP ensemble number */
  int32_t traceInEnsemble; /* Trace number within CDP ensemble */
  int16_t traceID; /* Trace identification code:
	1 = seismic data	4 = time break	7 = timing
	2 = dead		5 = uphole	8 = water break
	3 = dummy		6 = sweep	9..., optional use */
  int16_t vertSum, horSum;
  int16_t dataUse; /* 1 = production, 2 = test */
  int32_t sourceToRecDist;
  int32_t recElevation;
  int32_t sourceSurfaceElevation;
  int32_t sourceDepth;
  int32_t datumElevRec, datumElemSource;
  int32_t recWaterDepth, sourceWaterDepth;
  int16_t elevationScale;
  int16_t coordScale;
  int32_t sourceLongOrX, sourceLatOrY;
  int32_t recLongOrX, recLatOrY;
  int16_t coordUnits;
  int16_t weatheringVelocity;
  int16_t subWeatheringVelocity;
  int16_t sourceUpholeTime, recUpholeTime;
  int16_t sourceStaticCor, recStaticCor;
  int16_t totalStatic;
  int16_t lagTimeA, lagTimeB;
  int16_t delay;
  int16_t muteStart, muteEnd;
  int16_t sampleLength; /* Number of samples in this trace */
  int16_t deltaSample; /* Sampling interval in microseconds. */
  int16_t gainType; /* 1 = fixed, 2 = binary, 3 = floating, 4... opt.*/
  int16_t gainConst, initialGain;
  int16_t correlated; /* 1 = no, 2 = yes */
  int16_t sweepStart, sweepEnd;
  int16_t sweepLength; /* in milliseconds */
  int16_t sweepType; /* 1 = linear, 2 = parabolic, 3 = exponential, 4... opt. */
  int16_t sweepTaperAtStart, sweepTaperAtEnd;
  int16_t taperType;
  int16_t aliasFreq, aliasSlope;
  int16_t notchFreq, notchSlope;
  int16_t lowCutFreq, hiCutFreq;
  int16_t lowCutSlope, hiCutSlope;
  int16_t year, day, hour, minute, second;
  int16_t timeBasisCode;
  int16_t traceWeightingFactor;
  int16_t phoneRollPos1, phoneFirstTrace, phoneLastTrace;
  int16_t gapSize;
  int16_t taperOvertravel;
  int16_t extrash[10];
  int32_t  samp_rate;
  int16_t data_form, m_secs;
  int16_t trigyear,trigday,trighour,trigminute,trigsecond,trigmills;
  float scale_fac;
  int16_t inst_no;
  int16_t not_to_be_used;
  int32_t num_samps;
  char extra[8];
} SegyHead; /* end of segy trace header */

typedef struct SegyReel{
        int32_t            kjob, kline, kreel;
        int16_t           kntr, knaux, sr, kfldsr, knsamp, 
				kfsamp, dsfc, kmfold, ksort;
	unsigned char unused1[24];
	int16_t kunits;
	unsigned char unused2[344];
} SegyReel;
