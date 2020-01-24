<!--
// un/select table row
// setParentTR - unknown author - modified by Richard Standbrook
function getParent(el) {
	if (document.all)
		while (el.parentElement != null)
			if (el.tagName == 'TR') return el;
			else el = el.parentElement;
	else if (document.getElementById) {
		if (el.nodeType == 1 && el.tagName.toLowerCase() == 'tr') return el;
		else return getParent(el.parentNode);
	}
}

function setParentTR(e, CSSattr, CSSvalue, CSSvalue2) {
	var el = (e && e.srcElement) ? e.srcElement : (e && e.target) ? e.target : null;
	if (el) {
		var which = el.checked;
		var parentTR = getParent(el);
		parentTR.style[CSSattr] = (which) ? CSSvalue : CSSvalue2;
	}
}
// check all tick boxes
// selectAll - Richard Standbrook
function selectAll(frm) {
	for (var i=0;i < frm.length;i++) {
		fldObj = frm.elements[i]; // all form elements
		if (fldObj.type == 'checkbox') { // use only the checkboxes
			if(frm.checkall.checked == true) {
				fldObj.checked = true;
				/* change ot highlight the bg of each row with a checkbox*/
				if(fldObj.name != 'checkall') {
					var parentTR = getParent(fldObj);
					parentTR.style['background'] = (fldObj) ? '#C3D6E6' : '';
				}
			} else {
				fldObj.checked = false; 
				/* change the bg to a custom style="" property, newColor*/
				if(fldObj.name != 'checkall') {
					var parentTR = getParent(fldObj);
					parentTR.style.background = parentTR.style.originalColor;
				}
			}
		}
	}
}
// make the dynamic menus work in IE
startList = function() {
	if (document.all&&document.getElementById) {
		navRoot = document.getElementById("nav");
		for (i=0; i<navRoot.childNodes.length; i++) {
			node = navRoot.childNodes[i];
			if (node.nodeName=="LI") {
				node.onmouseover=function() {
					this.className+=" over";
				}
				node.onmouseout=function() {
					this.className=this.className.replace(" over", "");
				}
			}
		}
	}
}
window.onload=startList;

// Drop down menus for IE
// source:http://www.htmldog.com/articles/suckerfish/dropdowns/
sfHover = function() {
	var sfEls = document.getElementById("menus").getElementsByTagName("LI");
	for (var i=0; i<sfEls.length; i++) {
		sfEls[i].onmouseover=function() {
			this.className+=" sfhover";
		}
		sfEls[i].onmouseout=function() {
			this.className=this.className.replace(new RegExp(" sfhover\\b"), "");
		}
	}
}
if (window.attachEvent) window.attachEvent("onload", sfHover);

function queryInput( myValue ) {
	document.webdbeForm.webdbeQueryVal.value = myValue ;
}

//-->
