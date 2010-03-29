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

        // Set defaults for the colorscheme
        if( PlotSelect.canvasLineColor == undefined ) PlotSelect.canvasLineColor = "#FDFF4F";
        if( PlotSelect.canvasBgColor == undefined ) PlotSelect.canvasBgColor = "#00029F";
        if( PlotSelect.canvasTickColor === undefined ) PlotSelect.canvasTickColor = "#0009FF";
        if( PlotSelect.canvasSelection === undefined ) PlotSelect.canvasSelection = "#FFFFFF";

        // {{{ Change colorscheme

        $("select#cs").change(function(){

            var cs = $(this).val() ;

            if( cs === 'yb' ) {
                PlotSelect.canvasLineColor = "#FDFF4F";
                PlotSelect.canvasBgColor = "#00029F";
                PlotSelect.canvasTickColor = "#0009FF";
                PlotSelect.canvasSelection = "#FFFFFF";
            } else if( cs == 'bw' ) {
                PlotSelect.canvasLineColor = "#000000";
                PlotSelect.canvasBgColor = "#DBDBDB";
                PlotSelect.canvasTickColor = "#666666";
                PlotSelect.canvasSelection = "#666666";
            } else if( cs == 'yg' ) {
                PlotSelect.canvasLineColor = "#FDFF4F";
                PlotSelect.canvasBgColor = "#009F02";
                PlotSelect.canvasTickColor = "#00FF09";
                PlotSelect.canvasSelection = "#FFFFFF";
            } else if( cs == 'wb' ) {
                PlotSelect.canvasLineColor = "#FFFFFF";
                PlotSelect.canvasBgColor = "#000000";
                PlotSelect.canvasTickColor = "#666666";
                PlotSelect.canvasSelection = "#FFFFFF";
            } else if( cs == 'yp' ) {
                PlotSelect.canvasLineColor = "#FDFF4F";
                PlotSelect.canvasBgColor = "#660066";
                PlotSelect.canvasTickColor = "#993399";
                PlotSelect.canvasSelection = "#FFFFFF";
            } else if( cs == 'ob' ) {
                PlotSelect.canvasLineColor = "#FF6600";
                PlotSelect.canvasBgColor = "#000000";
                PlotSelect.canvasTickColor = "#666666";
                PlotSelect.canvasSelection = "#FFFFFF";
            } else {
                PlotSelect.canvasLineColor = "#FDFF4F";
                PlotSelect.canvasBgColor = "#00029F";
                PlotSelect.canvasTickColor = "#0009FF";
                PlotSelect.canvasSelection = "#FFFFFF";
            }
            $("span#csFg").css("background-color",PlotSelect.canvasLineColor);
            $("span#csBg").css("background-color",PlotSelect.canvasBgColor);

        });

        // }}} Change colorscheme

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
        PlotSelect.filterChange();
        PlotSelect.phaseSelector();
        // }}} Initialize functions

        // }}} Set defaults

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
        // var x1 = Math.round( pos.xaxis.from / 1000 ) ;
        // var x2 = Math.round( pos.xaxis.to / 1000 ) ;
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
        var dataargs = {"type":$("select#type").val()}

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
        } else {
            if( $("select#filter").length < 1 ) { // Need to determine if filters shown
                PlotSelect.getFilters();
            }
        }

        // Test for phases
        if( ( dataargs["phases"] !== undefined ) && ( dataargs['phases'] == 'True' ) ) {
            $("form#wformer input#phases").attr('checked','checked');
        }

        $("#loading").show();

        // PlotSelect.phaseSelector();

        // Define globally for app
        PlotSelect.type      = args.type;
        PlotSelect.stacode   = args.sta;
        PlotSelect.chan      = args.chan;
        PlotSelect.ts        = args.ts;
        PlotSelect.te        = args.te;
        PlotSelect.orid      = args.orid;
        PlotSelect.orid_time = args.orid_time;
        PlotSelect.amount    = args.amount;
        PlotSelect.phases    = args.phases;

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

        $.each(resp.sta, function(i, mysta){

            $.each(resp.chan, function(ii, mychan){


                stachan_data = mysta + '_' + mychan ; // Create the STA_CHAN data arrays from other response items
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
                        var plot = $.plot(chan_plot, [], opts0);
                } else if (typeof(resp[mysta][mychan]) == "undefined" ) { 
                        var plot = $.plot(chan_plot, [], opts0);
                } else {
                    
                    // This is the actual plotting
                    var flot_data = [];

                    if ( resp['type'] == "coverage") {

                        for (var start in resp[mysta][mychan] ) {
                            var start_time = parseFloat(start) *1000;
                            var end_time = parseFloat(resp[mysta][mychan][start]) *1000;
                            flot_data.push([start_time,3,end_time]);
                        }

                        opts0['yaxis']['min'] = 1
                        opts0['yaxis']['max'] = 6
                        opts0['bars'] = {show:true, horizontal:'true', barWidth:1};

                    }
                    else {
                        if (typeof(resp[mysta][mychan]['data']) == "undefined" ) { 
                            var plot = $.plot(chan_plot, [], opts0);
                        }
                        else {
                            //var st = resp[mysta][mychan]['start'];
                            //var et = resp[mysta][mychan]['end'];
                            //var period = (et-st)/resp[mysta][mychan]['data'].length;

                            if( resp[mysta][mychan]['format'] == 'bins' ) {

                                for ( var i=0, len=resp[mysta][mychan]['data'].length; i<len; ++i ){
                                    temp_data = resp[mysta][mychan]['data'][i];
                                    flot_data[i] =  [temp_data[0]*1000,temp_data[2],temp_data[1]];
                                }
                                opts0['bars'] = {show:true,barWidth:0,align:'center'};

                            }else {

                                for ( var i=0, len=resp[mysta][mychan]['data'].length; i<len; ++i ){
                                    temp_data = resp[mysta][mychan]['data'][i];
                                    flot_data[i] =  [temp_data[0]*1000,temp_data[1]];
                                }
                                opts0['lines'] = {show:true,lineWidth:2,shadowSize:4};

                            }
                        }
                    }

                    var plot = $.plot(chan_plot,[ flot_data ], opts0 );

                    // Bind and store
                    chan_plot.bind("plotselected", PlotSelect.handleSelect);
                    PlotSelect.chan_plot_obj[stachan_data] = plot;

                    // {{{ Add arrival labels
                    if( PlotSelect.phases !== undefined && PlotSelect.phases != 'False' && resp['phases'][stachan_data] !== undefined ) {

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

                } 

            });
        });

        if (typeof(resp['error']) != "undefined" ) {
            alert('ERROR ON SERVER:\n'+resp['error']);
        }

        $("#loading").fadeOut(500);

    },

    setEventData: function(resp){
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
