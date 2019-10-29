#ifndef _SEISMIC_PICK_H_
#define _SEISMIC_PICK_H_
#include "TimeWindow.h"
namespace SEISPP {
using namespace std;
using namespace SEISPP;

typedef struct PointPick {
        double time;
        double amplitude;
} PointPick;

enum PickType {POINT, WINDOW, PICKEND, UNDEFINED};

class SeismicPick {
public:
        PickType type;
        double time;
        double amplitude;
        int trace_number;
        TimeWindow twin;

        SeismicPick();
        SeismicPick(float x1in,float x2in);
        SeismicPick(TimeWindow tw);
        SeismicPick(const SeismicPick& p);
        SeismicPick& operator=(const SeismicPick& p);
        TimeWindow get_window();
        PointPick get_point();
        int get_trace_number();
        float get_x1() {return x1;}
        float get_x2() {return x2;}
        void set_point(double t, double a);
        friend class SeismicPlot;
private:
        float x1,x2;
        bool point_set;
        bool window_set;
};
}
#endif
