tickForm = function(val){
    var dt=new Date(val*1000); 
    //return dt.toLocaleString();
    var h = dt.getHours();
    var m = dt.getMinutes();
    if (m == 0) m="00";
    var s = dt.getSeconds();
    if (s == 0) s="00";
    return h + ":" + m + ":" + s;
};
nulltickForm = function(val){
    return "";
}


// Most key events are handled using: http://code.google.com/p/js-hotkeys/ 
PlotSelect = {

    isShiftPressed: false,

    init: function(){
        $(document).bind("keydown", "r", PlotSelect.resetPlot);
        $(document).bind("keydown", "left", PlotSelect.shiftPlotViewLeft);
        $(document).bind("keydown", "right", PlotSelect.shiftPlotViewRight);
        $(window).bind("keydown", PlotSelect.toggleShift);
        $(window).bind("keyup", PlotSelect.toggleShift);
        $("#station select").change(PlotSelect.handleStationChange);
        //Create initial plot with max values:
        PlotSelect.getData(0, 0, "all");
    },

    handleStationChange: function(evt){
        PlotSelect.getData(0, 0, "all");
    },

    resetPlot: function(evt){
        PlotSelect.getData(0, 0, "all");
    },

    shiftPlotViewRight: function(evt) {
        var firstchan = PlotSelect.chans[0]; //XXX Need to handle specific channels?
        var chanplot = PlotSelect.chan_plot_obj[firstchan]; 
        var xaxis = chanplot.getAxes().xaxis;
        var x1 = xaxis.datamin - xaxis.tickSize;
        var x2 = xaxis.datamax - xaxis.tickSize;
        PlotSelect.getData(x1, x2, "slice");
    },

    shiftPlotViewLeft: function(evt) {
        var firstchan = PlotSelect.chans[0]; //XXX Need to handle specific channels?
        var chanplot = PlotSelect.chan_plot_obj[firstchan]; 
        var xaxis = chanplot.getAxes().xaxis;
        var x1 = xaxis.datamin + xaxis.tickSize;
        var x2 = xaxis.datamax + xaxis.tickSize;
        PlotSelect.getData(x1, x2, "slice");
    },


    /*XXX Is there a better way to do this (toggleShift)?
     Currently this pre and post Shift toggling is done
     such that we can detect if Shift is pressed before
     we go into "PlotSelect.handleSelect"  which is an
     event handler that FLot passes plot click position data to.
    */
    toggleShift: function(evt) {
        PlotSelect.isShiftPressed = evt.shiftKey;
    },

    shiftGraph: function(dir){
        var glabels = $(".gridLabel");
        var v1 = $(glabels[0]).text();
        var v2 = $(glabels[1]).text();
    },


    addFlag: function(plotn, x, y){
        var ctx = plotn.getCanvas().getContext("2d");
        var img = new Image();
        img.src = "static/flag.png";
        //ctx.drawImage(img, x, y); //throws error sometimes, time issue?
    },

    handleSelect: function(evt, pos){
        var x1 = pos.x1;
        var x2 = pos.x2;
        if (PlotSelect.isShiftPressed) { /*if the Shift Key is pressed, we zoom out. */
            console.dir("out", PlotSelect.isShiftPressed);
            var pad = 5;
            var delta = x2 - x1;
            x1 = x1 - delta*pad;
            x2 = x2 + delta*pad;
        }
        PlotSelect.getData(x1, x2, "slice");
    },

    getData: function(time_start, time_end, amount){
        /*  x1,x2 -> the min and max of the selection.
            amount -> ('slice' || 'all' ) how much of the data to retrive.
        */
        $("#loading").show();
        var sta = $("#station select option:selected").val()
        $.ajax({
            type:"GET",
            dataType:"json",
            url:"data",
            data:{
                "ts":time_start, 
                "te":time_end, 
                "sta":sta 
                //"chan":["BHE", "BHN", "BHZ"], 
            },
            success:PlotSelect.setData
        });
    },

    setData: function(resp){ 
        var opts0 = {
            lines: {show:true, lineWidth:2, shadowSize:4},
            colors: ["#FDFF4F"], 
            selection: {mode:"x", color:"#FFF"}, 
            grid: {clickable:true, borderWidth:0, color:"#0009FF", tickColor:"#0009FF", backgroundColor:"#00029F"}, 
            xaxis: {noTicks:5, labelWidth:20, labelHeight:20, tickFormatter:nulltickForm},
            yaxis: {noTicks:4}
        };
        //deep copy and extend 'opts0'
        var opts1 = $.extend(true, opts0, {xaxis: {noTicks:5, tickFormatter:nulltickForm}});
        var opts2 = $.extend(true, opts0, {xaxis: {noTicks:5, tickFormatter:tickForm}});

        PlotSelect.chans = resp.chans;
        PlotSelect.chan_plot_obj = {}; //A mapping from a named Channel to it's associated 'flot' plot.
        
        var chan_labels = $("#chan_labels"), chan_plots = $("#chan_plots"); 
        chan_labels.empty();
        chan_plots.empty();
        $.each(resp.chans, function(i, chan){
            var lbltop = (i+1)*125; //Try to align chan labels with plot, could be done in a better way.
            var lbl = $("<div>").attr("id", chan+"_label").attr("class", "label").css("top", lbltop+"px").text(chan);
            var plt = $("<div>").attr("id", chan+"_plot").attr("class", "plot");
            chan_labels.append(lbl);
            chan_plots.append(plt);
            chan_plot = $("#"+chan+"_plot");
            var plot = $.plot(chan_plot, [resp[chan]], opts0);
            chan_plot.bind("selected", PlotSelect.handleSelect);
            PlotSelect.chan_plot_obj[chan] = plot;
        });
        $("#loading").fadeOut(500);
    }
};

$(document).ready(PlotSelect.init);
