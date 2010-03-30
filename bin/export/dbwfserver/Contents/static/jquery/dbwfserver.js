// Most key events are handled using: http://code.google.com/p/js-hotkeys/ 
PlotSelect = {

    isShiftPressed: false,

    init: function(sta){

        // {{{ Set defaults

        $(document).bind("keydown", "r", PlotSelect.resetPlot);
        $(document).bind("keydown", "left", PlotSelect.shiftPlotViewLeft);
        $(document).bind("keydown", "right", PlotSelect.shiftPlotViewRight);
        $(window).bind("keydown", PlotSelect.toggleShift);
        $(window).bind("keyup", PlotSelect.toggleShift);

        // Create initial plot with max values:
        $("#loading").hide();
        $("#wforms").hide();
        $("#interact").hide();

        // {{{ Define colorschemes

        PlotSelect.colorschemes = {};

        PlotSelect.colorschemes.yb = {
            lineColor : "#FDFF4F",
            bgColor   : "#00029F",
            tickColor : "#0009FF",
            selection : "#FFFFFF"
        };
        PlotSelect.colorschemes.bw = {
            lineColor : "#000000",
            bgColor   : "#DBDBDB",
            tickColor : "#666666",
            selection : "#666666"
        };
        PlotSelect.colorschemes.yg = {
            lineColor : "#FDFF4F",
            bgColor   : "#009F02",
            tickColor : "#00FF09",
            selection : "#FFFFFF"
        };
        PlotSelect.colorschemes.wb = {
            lineColor : "#FFFFFF",
            bgColor   : "#000000",
            tickColor : "#666666",
            selection : "#FFFFFF"
        };
        PlotSelect.colorschemes.yp = {
            lineColor : "#FDFF4F",
            bgColor   : "#660066",
            tickColor : "#993399",
            selection : "#FFFFFF"
        };
        PlotSelect.colorschemes.ob = {
            lineColor : "#FF6600",
            bgColor   : "#000000",
            tickColor : "#666666",
            selection : "#FFFFFF"
        }
        PlotSelect.colorschemes.def = {
            lineColor : "#FDFF4F",
            bgColor   : "#00029F",
            tickColor : "#0009FF",
            selection : "#FFFFFF"
        }

        // Set defaults for the colorscheme
        if( PlotSelect.canvasLineColor == undefined ) PlotSelect.canvasLineColor = PlotSelect.colorschemes.def.lineColor;
        if( PlotSelect.canvasBgColor == undefined ) PlotSelect.canvasBgColor = PlotSelect.colorschemes.def.bgColor;
        if( PlotSelect.canvasTickColor === undefined ) PlotSelect.canvasTickColor = PlotSelect.colorschemes.def.tickColor;
        if( PlotSelect.canvasSelection === undefined ) PlotSelect.canvasSelection = PlotSelect.colorschemes.def.selection;

        // }}} Define colorschemes

        // {{{ Setup AJAX defaults
        $.ajaxSetup({
            type: 'get',
            dataType: 'json',
            timeout: 120000,
            error:PlotSelect.errorResponse
        });

        // }}} Set defaults

        // {{{ Open the config panel
        $("a#configuration_open_link").click( function() {
            $("#configpanel").slideToggle("slow", function() {
                if( $(this).is(":hidden") ) {
                    $("a#configuration_open_link").html('Show configuration');
                } else {
                    $("a#configuration_open_link").html('Hide configuration');
                }
            });
        });
        // }}} Open the config panel

        // {{{ Canvas resize experiment
        // Not used yet
        // $(window).resize(function(){
        //     $('canvas').css({'width':'100%'});
        // });
        // }}} Canvas resize experiment

        // {{{ Arrival flag CSS
        PlotSelect.arrivalFlagCss = {
            'border':'1px solid #FFF',
            'background-color':'#F00',
            'font-weight':'bold',
            'font-size':'smaller',
            'color':'#FFF',
            'padding':'3px',
            'position':'absolute'
        };
        // }}} Arrival flag CSS

        // {{{ Initialize functions
        PlotSelect.urlParser();
        PlotSelect.colorschemeChange();
        PlotSelect.filterChange();
        PlotSelect.phaseSelector();
        PlotSelect.typeChange();
        // }}} Initialize functions

        // }}} Set defaults

    },

    urlParser: function(evt){

        // {{{ Get parts of URL

        var pathname = window.location.pathname;
        if( pathname !== undefined ) {
            var urlParts = pathname.split("/");
            if( urlParts[1] === "wfs" ){
                PlotSelect.urlPath = true;
            } else {
                PlotSelect.urlPath = false;
            }
        } else {
            PlotSelect.urlPath = false;
        }

        // }}} Get parts of URL

    },

    filterChange: function(evt){

        // {{{ Dynamic filter change data query

        dataObj = {
            type:PlotSelect.type,
            sta:PlotSelect.stacode,
            orid:PlotSelect.orid,
            orid_time:PlotSelect.orid_time,
            amount:PlotSelect.amount
        }

        if( PlotSelect.ts     !== undefined ) { dataObj['ts']     = parseInt(PlotSelect.ts,10) ; }
        if( PlotSelect.te     !== undefined ) { dataObj['te']     = parseInt(PlotSelect.te,10) ; }
        if( PlotSelect.phases !== undefined ) { dataObj['phases'] = PlotSelect.phases; }
        if( PlotSelect.chan   !== undefined ) { dataObj['chan']   = PlotSelect.chan; }

        $("form#wformer select#filter").change( function() {
            PlotSelect.myFilter = $(this).val();
            dataObj['filter'] = PlotSelect.myFilter ;
            $(this).attr("selected","selected");
            PlotSelect.getData(dataObj);
        });

        // }}} Dynamic filter change data query

    },

    colorschemeChange: function(evt){

        // {{{ Change colorscheme

        dataObj = {
            sta:PlotSelect.stacode,
            orid:PlotSelect.orid,
            orid_time:PlotSelect.orid_time,
            amount:PlotSelect.amount
        }

        if( PlotSelect.ts     !== undefined ) { dataObj['ts']     = parseInt(PlotSelect.ts,10) ; }
        if( PlotSelect.te     !== undefined ) { dataObj['te']     = parseInt(PlotSelect.te,10) ; }
        if( PlotSelect.filter !== undefined ) { dataObj['filter'] = PlotSelect.filter; }
        if( PlotSelect.chan   !== undefined ) { dataObj['chan']   = PlotSelect.chan; }


        $("select#cs").change(function(){

            var cs = $(this).val() ;

            PlotSelect.canvasLineColor = PlotSelect.colorschemes[cs]['lineColor'] ;
            PlotSelect.canvasBgColor   = PlotSelect.colorschemes[cs]['bgColor'];
            PlotSelect.canvasTickColor = PlotSelect.colorschemes[cs]['tickColor'];
            PlotSelect.canvasSelection = PlotSelect.colorschemes[cs]['selection'];

            $("span#csFg").css("background-color",PlotSelect.canvasLineColor);
            $("span#csBg").css("background-color",PlotSelect.canvasBgColor);

            if( PlotSelect.urlPath !== undefined || PlotSelect.urlPath !== false ) { PlotSelect.getData(dataObj); }

        });

        // }}} Change colorscheme

    },

    phaseSelector: function(evt){

        // {{{ Dynamic phase selector

        dataObj = {
            type:PlotSelect.type,
            sta:PlotSelect.stacode,
            orid:PlotSelect.orid,
            orid_time:PlotSelect.orid_time,
            amount:PlotSelect.amount
        }

        if( PlotSelect.ts     !== undefined ) { dataObj['ts']     = parseInt(PlotSelect.ts,10) ; }
        if( PlotSelect.te     !== undefined ) { dataObj['te']     = parseInt(PlotSelect.te,10) ; }
        if( PlotSelect.filter !== undefined ) { dataObj['filter'] = PlotSelect.filter; }
        if( PlotSelect.chan   !== undefined ) { dataObj['chan']   = PlotSelect.chan; }

        $("input#phases").change( function() {
            if( $(this).attr('checked') ) { 
                dataObj['phases'] = 'True' ;
            } else { 
                dataObj['phases'] = 'False' ;
            }
            PlotSelect.getData(dataObj);
        });
        // }}} Dynamic phase selector

    },

    typeChange: function(evt){

        // {{{ Dynamic type change data query

        dataObj = {
            sta:PlotSelect.stacode,
            orid:PlotSelect.orid,
            orid_time:PlotSelect.orid_time,
            amount:PlotSelect.amount
        }

        if( PlotSelect.ts     !== undefined ) { dataObj['ts']     = parseInt(PlotSelect.ts,10) ; }
        if( PlotSelect.te     !== undefined ) { dataObj['te']     = parseInt(PlotSelect.te,10) ; }
        if( PlotSelect.filter !== undefined ) { dataObj['filter'] = PlotSelect.filter; }
        if( PlotSelect.chan   !== undefined ) { dataObj['chan']   = PlotSelect.chan; }

        $("form#conftype select#type").change( function() {
            PlotSelect.type = $(this).val();
            dataObj['type'] = PlotSelect.type ;
            $(this).attr("selected","selected");

            if( PlotSelect.urlPath !== undefined || PlotSelect.urlPath !== false ) { PlotSelect.getData(dataObj); }

        });
        // }}} Dynamic type change data query

    },

    resetPlot: function(evt){

        // {{{ Reset plot
        dataObj = {
            type:PlotSelect.type,
            sta:PlotSelect.stacode,
            orid:PlotSelect.orid,
            orid_time:PlotSelect.orid_time,
            amount:'all'
        }

        if( PlotSelect.type   === undefined ) { dataObj['type']   = 'wf' ; }
        if( PlotSelect.phases !== undefined ) { dataObj['phases'] = PlotSelect.phases; }
        if( PlotSelect.filter !== undefined ) { dataObj['filter'] = PlotSelect.filter; }
        if( PlotSelect.chan   !== undefined ) { dataObj['chan']   = PlotSelect.chan; }

        PlotSelect.getData(dataObj);
        // }}} Reset plot

    },

    tickTranslator:function(tickSizeArr){

        // {{{ tickTranslator

        var increment = tickSizeArr[0], unit = tickSizeArr[1], factor=null;

        if( unit == 'hour' ) {
            factor = 3600 ;
        } else if( unit == 'minute' ) {
            factor = 60 ;
        } else if( unit == 'second' ) {
            factor = 1 ;
        } else {
            factor = 1 ;
        }

        return increment * factor ;

        // }}} tickTranslator

    },

    errorResponse:function(x,e) {

        // {{{ Report Errors to user

        if(x.status==0){

            alert('You are offline!!\n Please Check Your Network.');

        }else if(x.status==404){

            alert('Requested URL not found.');

        }else if(x.status==500){

            alert('Internel Server Error.');

        }else if(e=='parsererror'){

            alert('Error.\nParsing JSON Request failed.');

        }else if(e=='timeout'){

            alert('Request Time out.');

        }else {

            alert('Unknow Error.\n'+x.responseText);

        }

        // }}}

    },

    shiftPlotViewRight: function(evt) {

        // {{{ Future data

        var firstchan = PlotSelect.stacode[0]+'_'+PlotSelect.chans[0]; // Get the axis range from one plot
        var chanplot = PlotSelect.chan_plot_obj[firstchan]; 
        var xaxis = chanplot.getAxes().xaxis;
        var futureDelta = PlotSelect.tickTranslator( xaxis.tickSize );
        var x1 = parseInt((xaxis.datamin/1000) + futureDelta, 10);
        var x2 = parseInt((xaxis.datamax/1000) + futureDelta, 10);

        dataObj = {
            type:PlotSelect.type,
            sta:PlotSelect.stacode,
            orid:PlotSelect.orid,
            orid_time:PlotSelect.orid_time,
            ts:x1,
            te:x2,
            amount:"slice"
        }

        if( PlotSelect.filter !== undefined ) { dataObj['filter'] = PlotSelect.filter; }
        if( PlotSelect.phases !== undefined ) { dataObj['phases'] = PlotSelect.phases; }
        if( PlotSelect.chan   !== undefined ) { dataObj['chan']   = PlotSelect.chan; }

        PlotSelect.getData(dataObj);

        // }}} Future data

    },

    shiftPlotViewLeft: function(evt) {

        // {{{ Past data

        var firstchan = PlotSelect.stacode[0]+"_"+PlotSelect.chans[0]; // Get the axis range from one plot
        var chanplot = PlotSelect.chan_plot_obj[firstchan]; 
        var xaxis = chanplot.getAxes().xaxis;
        var pastDelta = PlotSelect.tickTranslator( xaxis.tickSize );
        var x1 = parseInt( (xaxis.datamin/1000) - pastDelta, 10 );
        var x2 = parseInt( (xaxis.datamax/1000) - pastDelta, 10 );

        dataObj = {
            type:PlotSelect.type,
            sta:PlotSelect.stacode,
            orid:PlotSelect.orid,
            orid_time:PlotSelect.orid_time,
            ts:x1,
            te:x2,
            amount:"slice"
        }

        if( PlotSelect.filter !== undefined ) { dataObj['filter'] = PlotSelect.filter; }
        if( PlotSelect.phases !== undefined ) { dataObj['phases'] = PlotSelect.phases; }
        if( PlotSelect.chan   !== undefined ) { dataObj['chan']   = PlotSelect.chan; }

        PlotSelect.getData(dataObj);

        // }}} Past data

    },


    /*XXX Is there a better way to do this (toggleShift)?
     Currently this pre and post Shift toggling is done
     such that we can detect if Shift is pressed before
     we go into "PlotSelect.handleSelect"  which is an
     event handler that Flot passes plot click position data to.
    */
    toggleShift: function(evt) {
        PlotSelect.isShiftPressed = evt.shiftKey;
    },

    shiftGraph: function(dir){
        var glabels = $(".gridLabel");
        var v1 = $(glabels[0]).text();
        var v2 = $(glabels[1]).text();
    },

    handleSelect: function(evt, pos){

        // {{{ Selection zoom functionality

        // Everything in milliseconds so divide by 1000 to get secs
        var x1 = parseInt( pos.xaxis.from / 1000, 10 ) ;
        var x2 = parseInt( pos.xaxis.to / 1000, 10 ) ;

        if (PlotSelect.isShiftPressed) { /*if the Shift Key is pressed, we zoom out. */
            var pad = 5;
            var delta = x2 - x1;
            x1 = x1 - delta*pad;
            x2 = x2 + delta*pad;
        }

        dataObj = {
            type:PlotSelect.type,
            sta:PlotSelect.stacode,
            orid:PlotSelect.orid,
            orid_time:PlotSelect.orid_time,
            amount:PlotSelect.amount,
            ts:x1,
            te:x2,
            amount:"slice"
        }

        if( PlotSelect.type   !== undefined ) { dataObj['type']   = 'wf'; }
        if( PlotSelect.filter !== undefined ) { dataObj['filter'] = PlotSelect.filter; }
        if( PlotSelect.phases !== undefined ) { dataObj['phases'] = PlotSelect.phases; }
        if( PlotSelect.chan   !== undefined ) { dataObj['chan']   = PlotSelect.chan; }

        PlotSelect.getData(dataObj);

        // }}} Selection zoom functionality

    },

    getData: function(passedArgsObj){

        // {{{ Get data

        // Defaults
        var args = {};

        if ( typeof(passedArgsObj) == "undefined" ) {
                alert("Function getData called with no arguments...");
                return 'ERROR calling getData()';
        }
        // Override defaults if necessary
        for( var argName in passedArgsObj ) {
            args[argName] = passedArgsObj[argName];
        }

        // Define the data object arguments
        var dataargs = {};

        if ('type' in args){        dataargs["type"]      = args.type ;}
        if ('sta' in args){         dataargs["sta"]       = args.sta ;}
        if ('orid' in args){        dataargs["orid"]      = args.orid ;}
        if ('orid_time' in args){   dataargs["orid_time"] = args.orid_time ;}
        if ('ts' in args){          dataargs["ts"]        = args.ts ;}
        if ('te' in args) {         dataargs["te"]        = args.te ;}
        if ('chan' in args) {       dataargs["chan"]      = args.chan ;}
        if ('amount' in args) {     dataargs["amount"]    = args.amount ;}
        if ('phases' in args) {     dataargs["phases"]    = args.phases ;}

        // Test if filter defined
        if( ( PlotSelect.myFilter !== undefined ) && ( PlotSelect.myFilter !== 'None' ) ) {
            dataargs["filter"] = PlotSelect.myFilter;
        }

        // Test for type over-ride
        if( dataargs["type"] !== $("select#type").val() ) {
            dataargs["type"] = $("select#type").val();
        }

        // Update phases checkbox
        if( ( dataargs["phases"] !== undefined ) && ( dataargs['phases'] == 'True' ) ) {
            $("form#wformer input#phases").attr('checked','checked');
        } else {
            $("form#wformer input#phases").attr('checked','');
        }

        // Override phases if coverage requested
        if( dataargs["type"] === 'coverage' ) {
            $("form#wformer input#phases").attr('checked','');
            dataargs["phases"] = 'False';
        } else {
            if( $("form#wformer input#phases").attr('checked') ) {
                dataargs["phases"] = 'True';
            } else {
                dataargs["phases"] = 'False';
            }
        }

        $("#loading").show();

        // Define globally for app
        PlotSelect.type      = dataargs.type;
        PlotSelect.stacode   = dataargs.sta;
        PlotSelect.chan      = dataargs.chan;
        PlotSelect.ts        = dataargs.ts;
        PlotSelect.te        = dataargs.te;
        PlotSelect.orid      = dataargs.orid;
        PlotSelect.orid_time = dataargs.orid_time;
        PlotSelect.amount    = dataargs.amount;
        PlotSelect.phases    = dataargs.phases;

        // Query
        $.ajax({
            type:'get',
            dataType:'json',
            url:"/data",
            data: dataargs,
            success:PlotSelect.setData,
            error:PlotSelect.errorResponse
        });

        // }}} Get data

    },

    setData: function(resp) {

        // {{{ Define graph defaults
        var opts0 = {
            colors: [PlotSelect.canvasLineColor], 
            selection: {mode:"x", color:PlotSelect.canvasSelection}, 
            grid: {clickable:true, borderWidth:0, color:PlotSelect.canvasTickColor, tickColor:PlotSelect.canvasTickColor, backgroundColor:PlotSelect.canvasBgColor},
            xaxis: {ticks:5, labelWidth:20, labelHeight:20, mode:"time", timeformat:"%H:%M:%S<br/>%y-%m-%d"},
            yaxis: {ticks:4, labelWidth:25}
        };
        // }}} Define graph defaults

        if (typeof(resp['type']) == "undefined" ) {
            alert("Sorry, query failed! Please retry or restart server");
            return;
        }

        PlotSelect.chans = resp.chan;
        PlotSelect.chan_plot_obj = {}; // A mapping from a named channel to it's associated 'flot' plot.
        PlotSelect.orid = resp.orid;

        var chan_labels = $("#chan_labels"), chan_plots = $("#chan_plots"); 
        chan_labels.empty();
        chan_plots.empty();

        // PlotSelect.tsMilli = opts0['xaxis']['min'] = resp['time_start'] * 1000;
        // PlotSelect.teMilli = opts0['xaxis']['max'] = resp['time_end'] * 1000;
        opts0['xaxis']['min'] = resp['time_start'] * 1000;
        opts0['xaxis']['max'] = resp['time_end'] * 1000;

        if( resp.sta === undefined ) {

            alert( 'You must choose a station to plot data for. Please press the "r" key to refresh the plots' ) ;

        } else {

            // {{{ Some data to plot

            $.each(resp.sta, function(sta_iterator,mysta){

                // {{{ Per station

                $.each(resp.chan, function(chan_iterator,mychan){

                    // {{{ Per channel

                    var stachan_data = mysta + '_' + mychan ; // Create the STA_CHAN data arrays from other response items
                    var wrapper = $("<div>").attr("id", stachan_data+"_wrapper").attr("class","wrapper");
                    var lbltxt = $("<p>").attr("class","chantitle").text(stachan_data);
                    var lbl = $("<div>").attr("id", stachan_data+"_label").attr("class", "label").append(lbltxt);
                    var plt = $("<div>").attr("id", stachan_data+"_plot").attr("class", "plot");
                    wrapper.append(lbl);
                    wrapper.append(plt);
                    chan_plots.append(wrapper);
                    chan_plot = $("#"+stachan_data+"_plot");

                    // Show plots
                    $("#wforms").show();
                    $("#interact").show();

                    if (typeof(resp[mysta]) == "undefined" ) { 

                        // {{{ No station defined

                        var flot_data = [];
                        opts0['yaxis']['min'] = 0;
                        opts0['yaxis']['max'] = 1;
                        flot_data[0] = resp['time_start'] *1000;
                        flot_data[1] = resp['time_end']   *1000;
                        var plot = $.plot(chan_plot, [ flot_data ], opts0);

                        // Bind and store
                        chan_plot.bind("plotselected", PlotSelect.handleSelect);
                        PlotSelect.chan_plot_obj[stachan_data] = plot;

                        // }}}  No station defined

                    } else if (typeof(resp[mysta][mychan]) == "undefined" ) { 

                        // {{{ No channel defined

                        var flot_data = [];
                        opts0['yaxis']['min'] = 0;
                        opts0['yaxis']['max'] = 1;
                        flot_data[0] = resp['time_start'] *1000;
                        flot_data[1] = resp['time_end']   *1000;
                        var plot = $.plot(chan_plot, [ flot_data ], opts0);

                        // Bind and store
                        chan_plot.bind("plotselected", PlotSelect.handleSelect);
                        PlotSelect.chan_plot_obj[stachan_data] = plot;

                        // }}} No channel defined

                    } else {
                    
                        // {{{ Plotting

                        var flot_data = [];

                        if ( resp['type'] == "coverage") {

                            // {{{ Coverage plot

                            if (typeof(resp[mysta][mychan]['data']) == "undefined" ) { 

                                var plot = $.plot(chan_plot, [], opts0);

                            } else {

                                $.each( resp[mysta][mychan]['data'], function(i,arr) {

                                    var start_time = parseFloat(arr[0],10) *1000;
                                    var end_time   = parseFloat(arr[1],10) *1000;
                                    flot_data.push([start_time,3,end_time]);

                                });

                            }

                            opts0['yaxis']['min'] = 1 ;
                            opts0['yaxis']['max'] = 6 ;
                            opts0['bars'] = {show:true, horizontal:'true', barWidth:1};

                            // }}} Coverage plot

                        } else {

                            // {{{ Waveform plot

                            if (typeof(resp[mysta][mychan]['data']) == "undefined" ) { 
                                var plot = $.plot(chan_plot, [], opts0);
                            } else {
                                //var st = resp[mysta][mychan]['start'];
                                //var et = resp[mysta][mychan]['end'];
                                //var period = (et-st)/resp[mysta][mychan]['data'].length;

                                if( resp[mysta][mychan]['format'] == 'bins' ) {

                                    for ( var i=0, len=resp[mysta][mychan]['data'].length; i<len; ++i ){
                                        temp_data = resp[mysta][mychan]['data'][i];
                                        flot_data[i] =  [temp_data[0]*1000,temp_data[2],temp_data[1]];
                                    }
                                    opts0['bars'] = {show:true,barWidth:0,align:'center'};

                                } else {

                                    for ( var i=0, len=resp[mysta][mychan]['data'].length; i<len; ++i ){
                                        temp_data = resp[mysta][mychan]['data'][i];
                                        flot_data[i] =  [temp_data[0]*1000,temp_data[1]];
                                    }
                                    opts0['lines'] = {show:true,lineWidth:2,shadowSize:4};

                                }
                            }

                            // }}} Waveform plot

                        }

                        var plot = $.plot(chan_plot,[ flot_data ], opts0 );

                        // Bind and store
                        chan_plot.bind("plotselected", PlotSelect.handleSelect);
                        PlotSelect.chan_plot_obj[stachan_data] = plot;

                        // {{{ Add arrival labels
                        if( PlotSelect.phases !== undefined && resp['type'] === "waveform" && PlotSelect.phases == 'True' && resp['phases'][stachan_data] !== undefined ) {

                            $.each(resp['phases'][stachan_data], function(phaseTime,phaseFlag){

                                var o;
                                o = plot.pointOffset( { x:(phaseTime*1000), y:1000 } ) ;
                                var flagCss = PlotSelect.arrivalFlagCss;
                                // Force override as we want bar almost to top
                                o.top = 20 ;

                                flagCss['left'] = o.left + 4 + "px" ;
                                flagCss['top'] = o.top + "px" ;
                                var arrDiv = $("<div>").css(flagCss).append( phaseFlag );

                                // Draw tail on arrival flag
                                var ctx = plot.getCanvas().getContext("2d");
                                ctx.beginPath();
                                o.left += 4;
                                ctx.moveTo(o.left,o.top);
                                ctx.lineTo(o.left,o.top + 120);
                                ctx.closePath();
                                ctx.lineWidth = 1;
                                ctx.strokeStyle = "#FFF";
                                ctx.stroke();

                                chan_plot.append(arrDiv);

                            });

                        }

                    // }}} Add arrival labels

                       // }}} Plotting

                    } 

                    // }}} Per channel

                });

                // }}} Per station

            });

            // }}} Some data to plot

        }

        if (typeof(resp['error']) != "undefined" ) {
            alert('ERROR ON SERVER:\n'+resp['error']);
        }

        $("#loading").fadeOut(500);

    },

    setEventData: function(resp){

        // Currently not used

        $('#subnav #event').empty();

        // {{{ Plot event table

        var event_metadata = '<table id="eventTable">';
        event_metadata += "<tr><th>Magnitude</th><td>"+resp['magnitude']+" "+resp['mtype']+"</td>";
        event_metadata += "<th>Date-Time</th><td>"+resp['readable_time']+"</td></tr>";
        event_metadata += "<tr><th>Location</th><td>"+resp['lat']+"&deg;N, "+resp['lon']+"&deg;E</td>";
        event_metadata += "<th>Depth</th><td>"+resp['depth']+"km</td></tr>";
        event_metadata += "<tr><th colspan='2'>Author</th><td colspan='2'>"+resp['auth']+" [Review: "+resp['review']+"]</td></tr>";
        event_metadata += "</table>";

        $('#subnav #event').append(event_metadata);

        // }}} Plot event table

    }

};

$(document).ready(PlotSelect.init);
