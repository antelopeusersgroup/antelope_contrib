// Most key events are handled using: http://code.google.com/p/js-hotkeys/ 
PlotSelect = {

    isShiftPressed: false,

    init: function(){

        // {{{ Set defaults

        $(document).bind("keydown", "r", PlotSelect.resetPlot);
        $(document).bind("keydown", "left", PlotSelect.shiftPlotViewLeft);
        $(document).bind("keydown", "right", PlotSelect.shiftPlotViewRight);
        $(window).bind("keydown", PlotSelect.toggleShift);
        $(window).bind("keyup", PlotSelect.toggleShift);

        $("div#nav ul li a").click(PlotSelect.parseQuery);
        $("div#subnav ul li a").click(PlotSelect.parseQuery);

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

        // {{{ jQuery UI interface functions

        // Initialize the dialog
        $("#config").dialog({
            bgiframe: true,
            autoOpen: false,
            height: 300,
            modal: false
        });
        // Open the dialog
        $("a#config_link").click( function() {
            $("#config").dialog("open");
        });
        // Initialize the dialog
        $("#info").dialog({
            bgiframe: true,
            autoOpen: false,
            height: 300,
            modal: false
        });
        // Open the dialog
        $("a#info_link").click( function() {
            $("#info").dialog("open");
        });
        // Initialize the dialog
        $("#themer").dialog({
            bgiframe: true,
            autoOpen: false,
            height: 300,
            modal: false
        });
        // Open the dialog
        $("a#themer_link").click( function() {
            $("#themer").dialog("open");
            PlotSelect.themerColorScheme;
        });

        // }}} jQuery UI interface functions

        // }}} Set defaults

    },

    doQueryAjax: function(dAUrl,dAType,dASta,dAOrid){

        // {{{ Process metadata query

        // Define query object
        var queryData = {} ;

        // Build the crumbs
        var crumbTrail ;

        queryData["type"] = dAType ;
        crumbTrail = dAType ;

        // Add methods and build crumb trail based on args
        if( dASta !== "-" ) {
            queryData["sta"] = dASta ; 
            crumbTrail += " &raquo; Stacode:" + dASta ;
        }
        if( dAOrid !== -1 ) {
            queryData["orid"] = dAOrid ; 
            crumbTrail += " &raquo; Orid:" + dAOrid ;
        }

        jQuery.ajax({
            type:'get',
            dataType:'json',
            url: dAUrl,
            data: queryData,
            error:PlotSelect.errorResponse,
            success: function(resp) {
                PlotSelect.printListResult(resp,dAType,dASta,dAOrid);
                PlotSelect.updateCrumbs(crumbTrail);
                $("#loading").fadeOut(500);
            }
        });

        // }}} Process metadata query

    },

    updateCrumbs: function(myCrumb){

        // {{{ Update HTML in crumbs

        $("#crumbspath").html(" &raquo; "+myCrumb);

        // }}} Update HTML in crumbs

    },

    getFilters: function(){

        // {{{ Dynamically get filters

        $.ajax({
            type:'get',
            dataType:'json',
            url:"data",
            data: {
                "type":"filters"
            },
            success:PlotSelect.populateFilters,
            error:PlotSelect.errorResponse
        });

        // }}} Dynamically get filters

    },

    populateFilters: function(fildata){

        // {{{ Populate filter in select boxes

        if (typeof(fildata['error']) != "undefined" ) {
            alert('ERROR ON SERVER:\n'+fildata['error']);
        }

        var themerFilters = '<p>Select a filter:<select id="filter" name="filter">\n';
        themerFilters += '<option value="None" selected="selected">None</option>';

        $.each(fildata, function(i,val){
            themerFilters += '<option value="'+val+'">'+i+'</option>';
        });
        themerFilters += '</select>';

        $("#subnav").append(themerFilters);

        //PlotSelect.filterChange();

        // }}} Populate filter in select boxes

    },

    filterChange: function(evt){

        // {{{ Dynamic filter change data query

        $("select#filter").change( function() {
            PlotSelect.myFilter = $(this).val();
            $(this).attr("selected","selected");
            if ( PlotSelect.stacode ) {
                //PlotSelect.getData({sta:PlotSelect.stacode,orid:PlotSelect.orid,time_start:PlotSelect.ts,time_end:PlotSelect.te,amount:"slice"});
            }
        });

        // }}} Dynamic filter change data query

    },

    parseQuery: function(){

        // {{{ Main parse metadata query

        // Reset the filter
        delete PlotSelect.myFilter;

        $("#loading").show();

        var qHrefParts = $(this).attr("href").split("?");
        var qUrl = qHrefParts[0];
        var qArgs = qHrefParts[1].split("&"); // Gets the args key/val pairs
        var qType, qSta, qOrid ; // Set default values

        $.each(qArgs, function(i,val){

            var qArgsKV = val.split("="); // Gets the args key/val pairs

            switch( qArgsKV[0] ) {
                case "type":
                    qType = qArgsKV[1];
                    break;
                case "sta":
                    qSta = qArgsKV[1];
                    break;
                case "orid":
                    qOrid = qArgsKV[1];
                    break;
                default:
                    break;
            }

        });

        if( typeof(qType) != 'undefined' && typeof(qSta) == 'undefined' && typeof(qOrid) == 'undefined' ) {
            PlotSelect.doQueryAjax(qUrl,qType,"-",-1);
        } else if( typeof(qType) != 'undefined' && typeof(qSta) != 'undefined' && typeof(qOrid) == 'undefined' ) {
            PlotSelect.doQueryAjax(qUrl,qType,qSta,-1);
        } else if( typeof(qType) != 'undefined' && typeof(qSta) == 'undefined' && typeof(qOrid) != 'undefined' ) {
            PlotSelect.doQueryAjax(qUrl,qType,"-",qOrid);
        } else if( typeof(qType) != 'undefined' && typeof(qSta) != 'undefined' && typeof(qOrid) != 'undefined' ) {
            PlotSelect.doQueryAjax(qUrl,qType,qSta,qOrid);
        }
        return false;

        // }}} Main parse metadata query

    },

    printListResult: function(resp,respType,respSta,respOrid){

        // {{{ Print list of stations or display event

        if (typeof(resp['error']) != "undefined" ) {
            alert('ERROR ON SERVER:\n'+resp['error']);
        } else {

            var count = 0;

            var temp_hash = [];

            var output = "<ul class='ui-helper-reset ui-helper-clearfix'>";

            if( respType === "events" && respSta !== "-" && respOrid !== -1 ) {

                $("#subnav").empty();

                PlotSelect.getData( {sta:respSta,orid:respOrid,amount:'all'} ) ;

            } else if( respType === "events" ) {

                if  (respOrid === -1) {
                    for (var x in resp) {
                        for (var i=0;i<resp[x].length;i++) {
                            temp_hash[resp[x][i]] = 1;
                        }
                    }
                } else {
                    for (var x in resp['phases']) {
                        temp_hash[x] = 1;
                    }
                }

                //temp_hash.sort();

                for (var i in temp_hash) {

                    output += "<li class='ui-state-active ui-corner-all'>";

                    if( respSta !== "-") {
                        output += "<a href='data?type=events&sta="+respSta+"&orid="+i+"'>"+i+"</a>";
                    } else if (respOrid !== -1){
                        output += "<a href='data?type=events&sta="+i+"&orid="+respOrid+"'>"+i+"</a>";
                    } else {
                        output += "<a href='data?type=events&orid="+i+"'>"+i+"</a>";
                    }

                    output += "</li>";

                    count++;

                    if( count % 20 == 0 ) output += "<br/>";
                }

            } else {

                // {{{ Create unordered list

                resp.sort();
                $.each(resp, function(i,val){

                    output += "<li class='ui-state-active ui-corner-all'>";
                    output += "<a href='data?type=events&sta="+val+"'>"+val+"</a>";
                    output += "</li>";

                    count++;

                    if( count % 20 == 0 ) output += "<br/>";

                });

                // }}} Create unordered list

            }

            output += "</ul>";

            $("#subnav").html(output);

            if (typeof(resp['phases']) != "undefined" ) {
                PlotSelect.setEventData(resp);
            } else {
                $('#event').empty();
            }


            PlotSelect.init();
            return false;

        }
        // }}} Print list of stations or display event

    },

    resetPlot: function(evt){

        // {{{ Reset plot

        var mySta = PlotSelect.stacode;
        var myOrid = PlotSelect.orid;
        PlotSelect.getData({sta:mySta,orid:myOrid,amount:'all'});

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

        var mySta = PlotSelect.stacode;
        var firstchan = PlotSelect.chans[0]; // Get the axis range from one plot
        var chanplot = PlotSelect.chan_plot_obj[firstchan]; 
        var xaxis = chanplot.getAxes().xaxis;
        var futureDelta = PlotSelect.tickTranslator( xaxis.tickSize );
        var x1 = (xaxis.datamin/1000) + futureDelta;
        var x2 = (xaxis.datamax/1000) + futureDelta;

        if( PlotSelect.orid !== undefined ) {

            PlotSelect.getData({sta:mySta,orid:PlotSelect.orid,time_start:x1,time_end:x2,amount:"slice"});

        } else {

            PlotSelect.getData({sta:mySta,time_start:x1,time_end:x2,amount:"slice"});

        }

        // }}} Future data

    },

    shiftPlotViewLeft: function(evt) {

        // {{{ Past data

        var mySta = PlotSelect.stacode;
        var firstchan = PlotSelect.chans[0]; // Get the axis range from one plot
        var chanplot = PlotSelect.chan_plot_obj[firstchan]; 
        var xaxis = chanplot.getAxes().xaxis;
        var pastDelta = PlotSelect.tickTranslator( xaxis.tickSize );
        var x1 = (xaxis.datamin/1000) - pastDelta;
        var x2 = (xaxis.datamax/1000) - pastDelta;

        if( PlotSelect.orid !== undefined ) {

            PlotSelect.getData({sta:mySta,orid:PlotSelect.orid,time_start:x1,time_end:x2,amount:"slice"});

        } else {

            PlotSelect.getData({sta:mySta,time_start:x1,time_end:x2,amount:"slice"});

        }

        // }}} Past data

    },


    /*XXX Is there a better way to do this (toggleShift)?
     Currently this pre and post Shift toggling is done
     such that we can detect if Shift is pressed before
     we go into "PlotSelect.handleSelect"  which is an
     event handler that Flot passes plot click position data to.
    */
    toggleShift: function(evt) {
        // console.log('SHIFT is pressed');
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
        var x1 = pos.xaxis.from / 1000 ;
        var x2 = pos.xaxis.to / 1000 ;

        if (PlotSelect.isShiftPressed) { /*if the Shift Key is pressed, we zoom out. */
            // console.log('Shift is pressed');
            // console.dir("out", PlotSelect.isShiftPressed);
            var pad = 5;
            var delta = x2 - x1;
            x1 = x1 - delta*pad;
            x2 = x2 + delta*pad;
        }

        PlotSelect.getData({sta:PlotSelect.stacode,orid:PlotSelect.orid,time_start:x1,time_end:x2,amount:"slice"});

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

        if ('type' in args){         dataargs["type"]    = args.type ;}

        if ('sta' in args){         dataargs["sta"]     = args.sta ;}
        if ('orid' in args){        dataargs["orid"]    = args.orid ;}
        if ('time_start' in args){  dataargs["ts"]      = args.time_start ;}
        if ('time_end' in args) {   dataargs["te"]      = args.time_end ;}
        if ('chan' in args) {       dataargs["chan"]    = args.chan ;}
        if ('amount' in args) {     dataargs["amount"]  = args.amount ;}

        // Test if filter defined
        if( ( PlotSelect.myFilter !== undefined ) && ( PlotSelect.myFilter !== 'None' ) ) {
            dataargs["filter"] = PlotSelect.myFilter;
        } else {
            if( $("select#filter").length < 1 ) { // Need to determine if filters shown
                PlotSelect.getFilters();
            }
        }

        $("#loading").show();

        // Define globally for app
        PlotSelect.stacode = args.sta;
        PlotSelect.orid = args.orid;

        // Query
        $.ajax({
            type:'get',
            dataType:'json',
            url:"data",
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

        PlotSelect.ts = opts0['xaxis']['min'] = resp['time_start'] * 1000;
        PlotSelect.te = opts0['xaxis']['max'] = resp['time_end'] * 1000;

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
                    var flot_data = new Array();

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

                } 

            });
        });

        if (typeof(resp['orid']) != "undefined" ) {

            dataargs = {};
            dataargs["type"]    = 'events' ;
            dataargs["orid"]    = resp['orid'] ;

            $.ajax({
                type:'get',
                dataType:'json',
                url:"data",
                data: dataargs,
                success:PlotSelect.setEventData,
                error:PlotSelect.errorResponse
            });
        }

        if (typeof(resp['error']) != "undefined" ) {
            alert('ERROR ON SERVER:\n'+resp['error']);
        }

        $("#loading").fadeOut(500);

    },

    setEventData: function(resp){
        $('#event').empty();

        // {{{ Plot event table

        var event_metadata = '<table id="eventTable">';
        event_metadata += "<tr><th>Magnitude</th><td>"+resp['magnitude']+" "+resp['mtype']+"</td></tr>";
        event_metadata += "<tr><th>Date-Time</th><td>"+resp['readable_time']+"</td></tr>";
        event_metadata += "<tr><th>Location</th><td>"+resp['lat']+"&deg;N, "+resp['lon']+"&deg;E</td></tr>";
        event_metadata += "<tr><th>Depth</th><td>"+resp['depth']+"km</td></tr>";
        event_metadata += "<tr><th>Author</th><td>"+resp['auth']+" [Review: "+resp['review']+"]</td></tr>";
        event_metadata += "</table>";

        $('#event').append(event_metadata);

        // }}} Plot event table

    }
//
//            // {{{ Add arrival labels
//
//        // Check the arrival labels are within the time window of the canvas
//        //    if( ( PlotSelect.ts > 0 && PlotSelect.te > 0 ) && ( resp['metadata']['phases'][mysta][mychan]['arrival_time'] > PlotSelect.ts ) && ( resp['metadata']['phases'][mysta][mychan]['arrival_time'] < PlotSelect.te ) ) {
//
//        //        var o;
//        //        o = plot.pointOffset( { x:(resp['metadata']['phases'][mysta][mychan]['arrival_time']*1000), y:1000 } ) ;
//        //        var arrCss = {
//        //            'border':'1px solid #FFF',
//        //            'background-color':'#F00',
//        //            'font-weight':'bold',
//        //            'font-size':'smaller',
//        //            'color':'#FFF',
//        //            'padding':'3px',
//        //            'position':'absolute'
//        //        };
//
//        //        // Force override as we want bar almost to top
//        //        o.top = 20 ;
//
//        //        arrCss['left'] = o.left + 4 + "px" ;
//        //        arrCss['top'] = o.top + "px" ;
//        //        var arrDiv = $("<div>").css(arrCss).append( resp['metadata']['phases'][mysta][mychan]['iphase'] );
//
//        //        // Draw tail on arrival flag
//        //        var ctx = plot.getCanvas().getContext("2d");
//        //        ctx.beginPath();
//        //        o.left += 4;
//        //        ctx.moveTo(o.left,o.top);
//        //        ctx.lineTo(o.left,o.top + 120);
//        //        ctx.closePath();
//        //        ctx.lineWidth = 1;
//        //        ctx.strokeStyle = "#FFF";
//        //        ctx.stroke();
//        //    
//        //        chan_plot.append(arrDiv);
//
//        //    }
//
//        //    // }}} Add arrival labels

};

$(document).ready(PlotSelect.init);
