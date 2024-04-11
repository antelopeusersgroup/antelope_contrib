/*
$Id$
*/

var myCookie = new Array() ; // Make sure we have at least an empty array

function initpop() {
	/*
	Function to automatically add popup window calls to each link
	with a target. Modified from:
	(c) http://icant.co.uk/forreview/popstuff/popstuff.js
	*/

	var windowAttributes='width=700,height=500,left=100,top=100,normal=false,status=no,resizable=yes,scrollbars=yes,toolbar=no,menubar=no,location=no';

	if(!window.opener) {
		var as,i,popfun
		as=document.getElementById('customize').getElementsByTagName('a');
		for (i=0;i<as.length;i++) {
			if(as[i].target) {
				popfun=function(){window.open(this.href,'',windowAttributes);return false;};
				as[i].onclick=popfun;
				as[i].onkeypress=popfun;
			}
		}
	}
}

function indexInArray( arr, val ) {
	for( var i=0;i<arr.length;i++ ) if( arr[i]==val) return i ;
	return -1 ;
}

function iterateForm() {
	if( document.getElementById('webdlmoncustomize') && document.getElementById('webdlmonCookie') ) {
		var array = myCookie ;
		for( var i=0; i < array.length; i++ ) {
			var spanId = 'co_'+array[i] ;
			var target = document.getElementById( spanId ) ;
			var targetInput = document.getElementById( array[i] ) ;
			targetInput.setAttribute( 'name', 'cookie['+i+']' ) ;
			var response = '('+i+')' ;
			target.innerHTML = response ;
		}
	}
}

function addToCookie( array, item ) {
	array.push( item )
	if( indexInArray( array, item ) == -1 ) {
		array.push( item )
	}
}

function removeFromCookie( array, item ) {
	for( var i=0; i<array.length; i++ ) {
		if( array[i] == item ) {
			var spanId = 'co_'+array[i] ;
			var target = document.getElementById( spanId ) ;
			var response = '' ;
			target.innerHTML = response ;
			array.splice(i, 1) ;
		}
	}
}

function setCookieOrder( field ) {
	var thisfield = document.getElementById( field ) ;
	if( thisfield.checked == true ) {
		addToCookie( myCookie, thisfield.value ) ;
		iterateForm( myCookie ) ;
	} else {
		removeFromCookie( myCookie, thisfield.value ) ;
		iterateForm( myCookie ) ;
	}
}

function clearFields( ) {
	inputs = document.getElementsByTagName('input') ;
	for( var i=0; i<inputs.length;i++ ) {
		removeFromCookie( myCookie, inputs[i].value )
		inputs[i].checked = false ;
	}
}

function targetParent( url ) {
	opener.location.href = url ;
	window.close() ;
}

// Close must have a reload or the new cookie will not be read
function closePopup( ) {
	opener.location.reload(true) ;
	window.close() ;
}

// With thanks to Scott Andrew
function addEvent(obj, evType, fn)
{ 
	if (obj.addEventListener)
	{ 
		obj.addEventListener(evType, fn, true); 
		return true; 
	} else if (obj.attachEvent){ 
		var r = obj.attachEvent("on"+evType, fn); 
		return r; 
	} else { 
		return false; 
	} 
}

// Load the init script for the popup
addEvent(window,'load',initpop);

// If the cookie array exists, load the init script for the customization
addEvent(window,'load',iterateForm);

