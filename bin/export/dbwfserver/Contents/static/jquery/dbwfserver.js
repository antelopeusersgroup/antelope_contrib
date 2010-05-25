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
        $("#tools").hide();

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
        PlotSelect.arrivalTailCss = {
            'position':'absolute',
            'border':'none',
            'border-left':'1px solid #FFF',
            'margin':'0',
            'padding':'0',
            'width':'1px'
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

    resetPlot: function(evt){

        location.reload(true);

    },

    filterChange: function(evt){

        // {{{ Dynamic filter change data query


        $("select#filter").change( function() {
            PlotSelect.getData();
        });


        // }}} Dynamic filter change data query

    },

    colorschemeChange: function(evt){

        // {{{ Change colorscheme

        $("select#cs").change(function(){

            var cs = $(this).val() ;

            PlotSelect.canvasLineColor = PlotSelect.colorschemes[cs]['lineColor'] ;
            PlotSelect.canvasBgColor   = PlotSelect.colorschemes[cs]['bgColor'];
            PlotSelect.canvasTickColor = PlotSelect.colorschemes[cs]['tickColor'];
            PlotSelect.canvasSelection = PlotSelect.colorschemes[cs]['selection'];

            $("span#csFg").css("background-color",PlotSelect.canvasLineColor);
            $("span#csBg").css("background-color",PlotSelect.canvasBgColor);

            PlotSelect.getData();

        });

        // }}} Change colorscheme

    },

    phaseSelector: function(evt){

        // {{{ Dynamic phase selector

        $("input#phases").change( function() {
            if( $(this).attr('checked') ) { 
                PlotSelect.phases = 'True' ;
            } else { 
                PlotSelect.phases = 'False' ;
            }
            $(".flag").toggle();
            $(".flagTail").toggle();
        });
        // }}} Dynamic phase selector

    },

    typeChange: function(evt){

        // {{{ Dynamic type change data query

        $("form#conftype select#type").change( function() {
            PlotSelect.type = $(this).val();
            PlotSelect.getData();

        });
        // }}} Dynamic type change data query

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

            alert('You are offline!!\n Please Check Your Network.' + '\n\n' + e);

        }else if(x.status==404){

            alert('Requested URL not found.' + '\n\n' + e);

        }else if(x.status==500){

            alert('Internel Server Error.' + '\n\n' + e);

        }else if(e=='parsererror'){

            alert('Error.\nParsing JSON Request failed.' + '\n\n' + e);

        }else if(e=='timeout'){

            alert('Request Time out.' + '\n\n' + e);

        }else {

            //alert('Error.\n'+ x.responseText);
            alert('Error:'+ x + '\n\n' + e);

        }

        // }}}

    },

    shiftPlotViewRight: function(evt) {

        // {{{ Future data

        var delta = PlotSelect.te - PlotSelect.ts ;

        dleta = delta/4;

        PlotSelect.ts += delta;

        PlotSelect.te += delta;

        PlotSelect.getData();

        // }}} Future data

    },

    shiftPlotViewLeft: function(evt) {

        // {{{ Past data

        var delta = PlotSelect.te - PlotSelect.ts ;

        dleta = delta/4;

        PlotSelect.ts -= delta;

        PlotSelect.te -= delta;

        PlotSelect.getData();

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

    handleSelect: function(evt, pos){

        // {{{ Selection zoom functionality

        if (PlotSelect.isShiftPressed) { /*if the Shift Key is pressed, we zoom out. */
            var delta = 0;

            delta = parseInt( pos.xaxis.from / 1000, 10 ) - PlotSelect.ts ;

            PlotSelect.ts -= delta;

            delta = PlotSelect.te - parseInt( pos.xaxis.to / 1000, 10 ) ;

            PlotSelect.te += delta;

        }
        else { 

            PlotSelect.ts = parseInt( pos.xaxis.from / 1000, 10 ) ;

            PlotSelect.te = parseInt( pos.xaxis.to / 1000, 10 ) ;

        }

        PlotSelect.getData();

        // }}} Selection zoom functionality

    },

    getData: function(args){

        // {{{ Get data

        if ( typeof(args) == "undefined" ) {
            // PlotSelect is define globally for app
            args = {};

            args.type   = PlotSelect.type,
            args.stas   = PlotSelect.stas,
            args.chans  = PlotSelect.chans,
            args.ts     = PlotSelect.ts,
            args.te     = PlotSelect.te,
            args.phases = PlotSelect.phases
        }

        // Test if filter defined
        if( args.filter !== $("select#filter option:selected").val() ) {
            args.filter = $("select#filter option:selected").val();
        }

        // Test for type over-ride
        if( args.type !== $("select#type").val() ) {
            args.type = $("select#type").val();
        }

        // Update phases checkbox
        if(  args.phases == 'True' ) {
            $("form#wformer input#phases").attr('checked','checked');
        } else {
            $("form#wformer input#phases").attr('checked','');
        }

        // Override phases if coverage requested
        if( args.type === 'coverage' ) {
            $("form#wformer input#phases").attr('checked','');
            args.phases = 'False';
        } else {
            if( $("form#wformer input#phases").attr('checked') ) {
                args.phases = 'True';
            } else {
                args.phases = 'False';
            }
        }

        $("#loading").show();


        // Query
        // Change arrays to strings w/ + 
        var sta_list = ''; 
        var chan_list = ''; 

        $.each(args.stas, function(iterator,mysta){
            sta_list = sta_list + '+' + mysta ;
        });
        $.each(args.chans, function(iterator,mychan){
            chan_list = chan_list + '+' + mychan ;
        });
        // use substring(1) to remove first character '+' on new string


        $.ajax({
            type:'get',
            dataType:'json',
            url:"/data/"+args.type+"/"+sta_list.substring(1)+"/"+chan_list.substring(1)+"/"+args.ts+"/"+args.te+"/"+args.filter ,
            success:PlotSelect.setData,
            error:PlotSelect.errorResponse
        });

        // }}} Get data

        $("#loading").hide();

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

        // Define globally for app
        PlotSelect.ts       = resp['time_start'];
        PlotSelect.te       = resp['time_end'];
        PlotSelect.type     = resp['type'];
        PlotSelect.stas     = resp.sta;
        PlotSelect.chans    = resp.chan;
        PlotSelect.orid     = resp.orid;

        PlotSelect.chan_plot_obj = {}; // A mapping from a named channel to it's associated 'flot' plot.

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
                    //wrapper.append(lbl);
                    //wrapper.append(plt);
                    //chan_plots.append(wrapper);
                    //chan_plot = $("#"+stachan_data+"_plot");


                    // Show plots
                    $("#wforms").show();
                    $("#interact").show();

                    if (typeof(resp[mysta]) == "undefined" ) { 

                        // {{{ No station defined

                        //
                        // This station is not valid.
                        // Avoid plotting...
                        //
                        //var flot_data = [];
                        //opts0['yaxis']['min'] = 0;
                        //opts0['yaxis']['max'] = 1;
                        //flot_data[0] = resp['time_start'] *1000;
                        //flot_data[1] = resp['time_end']   *1000;
                        //var plot = $.plot(chan_plot, [ flot_data ], opts0);

                        // Bind and store
                        //chan_plot.bind("plotselected", PlotSelect.handleSelect);
                        //PlotSelect.chan_plot_obj[stachan_data] = plot;

                        // }}}  No station defined

                    } else if (typeof(resp[mysta][mychan]) == "undefined" ) { 

                        // {{{ No channel defined

                        //
                        // This channel is not valid for the station. 
                        // Avoid plotting...
                        //
                        //var flot_data = [];
                        //opts0['yaxis']['min'] = 0;
                        //opts0['yaxis']['max'] = 1;
                        //flot_data[0] = resp['time_start'] *1000;
                        //flot_data[1] = resp['time_end']   *1000;
                        //var plot = $.plot(chan_plot, [ flot_data ], opts0);

                        // Bind and store
                        //chan_plot.bind("plotselected", PlotSelect.handleSelect);
                        //PlotSelect.chan_plot_obj[stachan_data] = plot;

                        // }}} No channel defined

                    } else {
                    
                        // {{{ Plotting

                        wrapper.append(lbl);
                        wrapper.append(plt);
                        chan_plots.append(wrapper);
                        chan_plot = $("#"+stachan_data+"_plot");

                        var flot_data = [];

                        if ( resp['type'] == "coverage") {

                            // {{{ Coverage plot

                            if (typeof(resp[mysta][mychan]['data']) == "undefined" ) { 

                                var plot = $.plot(chan_plot, [], opts0);

                            } else {

                                $.each( resp[mysta][mychan]['data'], function(i,arr) {

                                    var start_time = parseFloat(arr[0],10) *1000;
                                    var end_time   = parseFloat(arr[1],10) *1000;
                                    flot_data.push([start_time,1,end_time]);

                                });

                            }

                            opts0['yaxis']['ticks'] = 0;
                            opts0['yaxis']['min'] = 0.8 ;
                            opts0['yaxis']['max'] = 2.2 ;
                            opts0['bars'] = {show:true, horizontal:'true', barWidth:1};

                            var plot = $.plot(chan_plot,[ flot_data ], opts0 );

                            // }}} Coverage plot

                        } else {

                            // {{{ Waveform plot

                            if (typeof(resp[mysta][mychan]['data']) == "undefined" ) { 
                                var plot = $.plot(chan_plot, [], opts0);

                                var flagCss = {};
                                flagCss['color'] = 'red';
                                flagCss['position'] = 'absolute';
                                flagCss['left'] =  '250px';
                                flagCss['bottom'] = '50px';
                                flagCss['font-size'] = 'large';
                                var arrDiv = $("<div>").css(flagCss).append('No data in time segment: ('+resp['time_start']+','+resp['time_end']+').');

                                chan_plot.append(arrDiv);

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
                                    opts0['lines'] = {show:false};

                                } else {

                                    for ( var i=0, len=resp[mysta][mychan]['data'].length; i<len; ++i ){
                                        temp_data = resp[mysta][mychan]['data'][i];
                                        flot_data[i] =  [temp_data[0]*1000,temp_data[1]];
                                    }
                                    opts0['lines'] = {show:true,lineWidth:2,shadowSize:4};
                                    opts0['bars'] = {show:false};

                                }

                                var plot = $.plot(chan_plot,[ flot_data ], opts0 );

                            }

                            // }}} Waveform plot

                        }

                        // Get the size of the screen
                        //var p_width  = $("#wforms").width();

                        // Resize waveform and label
                        //if (p_width) {
                        //    //$("#"+stachan_data+"_label").width(p_width*0.09);
                        //    //$("#"+stachan_data+"_plot").width(p_width*0.80);
                        //}


                        if ( resp['type'] == "coverage") {
                            //$("canvas").css("height","20px");
                            //$(".label").css("height","20px");
                            //$(".plot").css("height","20px");
                            //$(".wrapper").css("height","20px");
                        }

                        // Bind and store
                        chan_plot.bind("plotselected", PlotSelect.handleSelect);
                        PlotSelect.chan_plot_obj[stachan_data] = plot;

                        // {{{ Add arrival labels

                        if( resp['phases'] !== undefined && resp['phases'] !== null ) {
                            if( resp['phases'][stachan_data] !== undefined && ( $("input#phases").attr('checked') == true || PlotSelect.phases == 'True' ) ) {

                                $.each(resp['phases'][stachan_data], function(phaseTime,phaseFlag){

                                    var o = plot.pointOffset( { x:(phaseTime*1000), y:1000 } ) ;

                                    var flagTop = plot.getPlotOffset() ;

                                    var flagCss = PlotSelect.arrivalFlagCss;
                                    flagCss['left'] = o.left + "px" ;
                                    flagCss['top'] = flagTop.top + "px" ;

                                    var flagTail = PlotSelect.arrivalTailCss;
                                    flagTail['left'] = flagCss['left'] ;
                                    flagTail['top'] = flagCss['top'] ;
                                    flagTail['height'] = plot.height() + 'px';

                                    var arrDiv = $("<div class='flag'>").css(flagCss).append( phaseFlag );
                                    var arrTailDiv = $("<div class='flagTail'>").css(flagTail);

                                    chan_plot.append(arrDiv);     // Flag
                                    chan_plot.append(arrTailDiv); // Flag tail

                                });
                            }
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
        

        $("#tools").show();

    },

    setEventData: function(resp){

        $('#subnav #event').empty();

        // {{{ Plot event table

        var event_metadata = '<table id="eventTable">';
        event_metadata += "<tr><th>Magnitude</th><td>"+resp['magnitude']+" "+resp['mtype']+"</td>";
        event_metadata += "<th>Date-Time</th><td>"+resp['time']+"</td></tr>";
        event_metadata += "<tr><th>Location</th><td>"+resp['lat']+"&deg;N, "+resp['lon']+"&deg;E</td>";
        event_metadata += "<th>Depth</th><td>"+resp['depth']+"km</td></tr>";
        event_metadata += "<tr><th>Author</th><td>"+resp['auth']+"</td><th>nass</th><td>"+resp['nass']+"</td></tr>";
        event_metadata += "</table>";

        $('#subnav #event').append(event_metadata);

        // }}} Plot event table

    },

    buildSelect: function(s_list,e_list,st,ev){

//{{{
       var subnavcontent = '';

       if (s_list && ev) {
            s_list = s_list.sort() ;
            subnavcontent = '<ul class="ui-helper-reset ui-helper-clearfix">';
            jQuery.each(s_list, function() {
                subnavcontent += "<li class='ui-state-active ui-corner-all'>" + "<a href='/wf/" + this + "/" + ev + "'>" + this + "</a></li>\n";
            });
            subnavcontent += '</ul>';
            $("#subnavcontent").append(subnavcontent);
        }

        else if (s_list) {
            s_list = s_list.sort() ;
            subnavcontent = '<ul class="ui-helper-reset ui-helper-clearfix">';
            jQuery.each(s_list, function() {
                subnavcontent += "<li class='ui-state-active ui-corner-all'>" + "<a href='/stations/" + this + "'>" + this + "</a></li>\n";
            });
            subnavcontent += '</ul>';
            $("#subnavcontent").append(subnavcontent);
        }

        else if (e_list && st) {

            sorted_e_list = [];
            table_headers = [];

            jQuery.each(e_list, function(key,value) {
                sorted_e_list.push(key);
                jQuery.each( value, function(sKey,sVal) {
                    if( jQuery.inArray(sKey,table_headers) == -1 ) { table_headers.push(sKey); }
                });
            });
            sorted_e_list = sorted_e_list.sort();
            table_headers = table_headers.sort();

            subnavcontent = '<table id="evsTbl" class="evListTable">';

            subnavcontent += '<thead><tr>\n';
            subnavcontent += '<th>time</th>\n';
            jQuery.each(table_headers, function(thi, thv) {
                if( thv !== 'time' ) {
                    subnavcontent += '<th>'+thv+'</th>\n';
                }
            });
            subnavcontent += '</tr></thead><tbody>\n';

            jQuery.each(sorted_e_list, function(key, value) {
                subnavcontent += "<tr>";
                var tbl_date = new Date(e_list[value]['time'] * 1000);
                subnavcontent += "<td><span style='display:none;'>" + e_list[value]['time'] * 1000 + "</span><a href='/wf/" + st + "/" + value + "'>" + tbl_date + "</a></td>";
                jQuery.each(table_headers, function(thi, thv) { 
                    if( thv !== 'time' ) {
                        subnavcontent += "<td>" + e_list[value][thv] + "</td>";
                    }
                });
                subnavcontent += "</tr>";
            });
            subnavcontent += '</tbody></table>';
            $("#subnavcontent").append(subnavcontent);
            $("#subnavcontent #evsTbl").tablesorter( {sortList: [[0,0], [1,0]]} );
        }

        else if (e_list) {

            sorted_e_list = [];
            table_headers = [];

            jQuery.each(e_list, function(key,value) {
                sorted_e_list.push(key);
                jQuery.each( value, function(sKey,sVal) {
                    if( jQuery.inArray(sKey,table_headers) == -1 ) { table_headers.push(sKey); }
                });
            });
            sorted_e_list = sorted_e_list.sort();
            table_headers = table_headers.sort();

            subnavcontent = '<table id="evsTbl" class="evListTable">\n';

            subnavcontent += '<thead><tr>\n';
            subnavcontent += '<th>time</th>\n';
            jQuery.each(table_headers, function(thi, thv) {
                if( thv !== 'time' ) {
                    subnavcontent += '<th>'+thv+'</th>\n';
                }
            });
            subnavcontent += '</tr></thead><tbody>\n';

            jQuery.each(sorted_e_list, function(key, value) {
                subnavcontent += "<tr>";
                var tbl_date = new Date(e_list[value]['time'] * 1000);
                subnavcontent += "<td><span style='display:none;'>" + e_list[value]['time'] * 1000 + "</span><a href='/events/" + value + "'>" + tbl_date + "</a></td>";
                jQuery.each(table_headers, function(thi, thv) { 
                    if( thv !== 'time' ) {
                        subnavcontent += "<td>" + e_list[value][thv] + "</td>";
                    }
                });
                subnavcontent += "</tr>";
            });
            subnavcontent += '</tbody></table>';
            $("#subnavcontent").append(subnavcontent);
            $("#subnavcontent #evsTbl").tablesorter( {sortList: [[0,0], [1,0]]} );
        }

//}}}

    }

};

$(document).ready(PlotSelect.init);
