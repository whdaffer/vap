/////////////////////////////////////////////////////////////
//
// base.js - base functions for cross-browser api
//
/////////////////////////////////////////////////////////////
/*
The contents of this file are subject to the Netscape Public
License Version 1.1 (the "License"); you may not use this file
except in compliance with the License. You may obtain a copy of
the License at http://www.mozilla.org/NPL/

Software distributed under the License is distributed on an "AS
IS" basis, WITHOUT WARRANTY OF ANY KIND, either express or
implied. See the License for the specific language governing
rights and limitations under the License.

The Initial Developer of the Original Code is Bob Clary.

Contributor(s): Bob Clary, Original Work, Copyright 1999-2000
                Bob Clary, Netscape Communications, Copyright 2001

Alternatively, the contents of this file may be used under the
terms of the GNU Public License (the "GPL"), in which case the
provisions of the GPL are applicable instead of those above.
If you wish to allow use of your version of this file only
under the terms of the GPL and not to allow others to use your
version of this file under the NPL, indicate your decision by
deleting the provisions above and replace them with the notice
and other provisions required by the GPL.  If you do not delete
the provisions above, a recipient may use your version of this
file under either the NPL or the GPL.
*/

// keep track of included source files
//
var _REGISTEREDFILES	= new Object(); 
registerFile('base.js');

// check that the required files are included
function registerFile(fileName, requiredFiles)
{
	var i;
	var missingFiles	= new Array();
	
	if (typeof(requiredFiles) == 'string')
		_REGISTEREDFILES[fileName] = requiredFiles.split(/,\s/);
	else
		_REGISTEREDFILES[fileName] = new Array();
		
	checkFiles(fileName, missingFiles);
			
	if (missingFiles.length != 0)
	{
		var hash = new Object();
		var msg = 'File ' + fileName + ' is missing required File' + (missingFiles.length > 1 ? 's': '');
		var p;
		
		for (i = 0; i < missingFiles.length; i++)
			hash[missingFiles[i]] = true;
			
		for (p in hash)
			msg += ' ' + p + ',';
			
		msg = msg.substring(0, msg.length-1);
	
		reportError(msg);
	}
	
	function checkFiles(fileName, missingFiles)
	{
		var fileRef = _REGISTEREDFILES[fileName];
			
		if (typeof(fileRef) == 'undefined')
		{
			missingFiles[missingFiles.length] = fileName;
			return;
		}
				
		for (var i = 0; i < fileRef.length; i++)
			checkFiles(fileRef[i], missingFiles);
			
		return;
	}
}

// dynamically loaded scripts
//
var LOADSCRIPTSUPPORTED = 'unknown';
function checkScriptLoadSupported(pathToTestScriptLoad)
{
	document.write('<script language="javascript" src="' + pathToTestScriptLoad + '"><\/script>');
}

// it is an error to reference anything from the dynamically loaded file inside the
// same script block.  This means that a file can not check its dependencies and
// load the files for it's own use.  someone else must do this.	
	
function loadScript(fileName)
{
/*
	// test if browser supports dynamic script loading.
	if (LOADSCRIPTSUPPORTED != 'yes')
	{
		reportError('your browser does not support dynamic script loading. Sorry.');
	}
*/
	document.write('<script language="javascript" src="' + fileName + '"><\/script>');
}

// get current working directory 
function getCurrentDirectory()
{
	var path = document.location.pathname;
	var i    = path.lastIndexOf('/');
	
	if (i > 0)
		path = path.substring(0, i + 1);
		
	path = document.location.protocol + '//' + document.location.host + path;
		
	return path;
}
	

// Navigation Stuff
// XXX: there has to be a better way...

// return object with site data { server: string, root: string }
// XXX:
function getSiteData()
{
	var data = { server: '', root: '' };
	
	data.server = document.location.protocol + '//' + document.location.host;
	data.root   = document.location.pathname.substring(0, document.location.pathname.indexOf('/', 1));
	if (!data.root)
		data.root = '';

	xbdump('getSiteData server=' + data.server + ' root=' + data.root);
	return data;
}


function framesetIs(page)
{
	if (self == top)
		top.location.href = page;
}

// bail out!
function goHome()
{
	var siteData = getSiteData();
	var myPath;

	myPath   = siteData.server + siteData.root + '/';

	top.document.location.href =  myPath;
}

// Extend Javascript Objects

// Function
//
// functionToString() is a replacement for the standard Function.toString()
// so that only the function name and argument list are returned rather than
// all of the source code for the function.
function functionToString()
{
	var s = this.oldToString();
	var i;
	var t;
	
	if (s == null || typeof(s) != 'string' || s.length == 0)
		return '';
	
	i = s.indexOf('native code');
	if (i != -1  && s.indexOf('functionToString') == -1)
		return s;
		
	i = s.indexOf(')');
	if (i == -1)
		t = s;
	else
		t = s.substr(0, i+1) + '{ [user code] }';
		
	return t;
}

Function.prototype.oldToString = Function.prototype.toString;
Function.prototype.toString    = functionToString;

// String
//

function stringToInteger(s)
{
	return parseInt(('0' + s).replace(/[^0-9]/g,''), 10)
}

function stringToFloat(s)
{
	return parseFloat(('0' + s).replace(/[^0-9]/g,''), 10)
}

function escapeHTML()
{
	var s = this.toString();
	
	s = s.replace(/\&/g, '&amp;');
	s = s.replace(/\</g, '&lt;');
	s = s.replace(/\>/g, '&gt;');
	s = s.replace(/ /g,  '&nbsp;');
	s = s.replace(/\n/g, '<br>');
    
	return s;
}

function unescapeHTML()
{
	var s = this.toString();
	
	s = s.replace(/<br>/g, '\n');
	s = s.replace(/\&nbsp;'/g, ' ');
	s = s.replace(/\&lt;/g,  '<');
	s = s.replace(/\&gt;/g,  '>');
	s = s.replace(/\&amp;/g, '&');
	
	return s;
}
	
String.prototype.escapeHTML   = escapeHTML;
String.prototype.unescapeHTML = unescapeHTML;
String.prototype.toInteger    = stringToInteger;
String.prototype.toInteger    = stringToFloat;

// IE 5.01 does not define a variable undefined whose value is undefined,
// but we can hack it by defining it anyway!
var undefined;

// IE 5.01 (and perhaps IE 4.x do not support (p in obj) boolean test
// IE 5.01 (at least) window, document objects are not JS objects with a prototype, 
// so the only portable means is to use a function rather than a method

function hasProperty(obj, p) 
{ 
	return typeof(obj[p]) != 'undefined';
}

// DEBUG Stuff
//	
var DEBUG = false;
var DUMPWINDOW = null;

if (DEBUG)
	window.onunload = xbdumpclose;

function xbdumpopen()
{
	if (!DEBUG)
		return;
		
	if (DUMPWINDOW)
		DUMPWINDOW.close();
		
	DUMPWINDOW = window.open('about:blank', 'DUMPWINDOW', 'height=400,width=600,resizable=yes,scrollbars=yes');
	DUMPWINDOW.title = 'Javascript Debug Window';
	DUMPWINDOW.document.write('<html><head><title>JavaScript Debug Window</title></head><body><h3>Javascript Debug Window</h3></body></html>');
	DUMPWINDOW.focus();
}

function xbdumpclose()
{
	if (!DEBUG)
		return;
		
	if (DUMPWINDOW && !DUMPWINDOW.closed)
		DUMPWINDOW.close();
}

function xbdump(msg)
{
	if (!DEBUG)
		return;
		
	if (!DUMPWINDOW || DUMPWINDOW.closed)
		xbdumpopen();
		
	DUMPWINDOW.document.write('<br>' + msg);
	
	return;
}


var LOGELEMENT  = null;

function writeLogWindow(message)
{
	if (!LOGELEMENT)
		LOGELEMENT = document.getElementById('LOGWINDOW');
	
	var p = document.createElement('br');
	var t = document.createTextNode(message);
	
	LOGELEMENT.appendChild(t);
	LOGELEMENT.appendChild(p);
}

function clearLogWindow(evt)
{
	if (!LOGELEMENT)
		LOGELEMENT = document.getElementById('LOGWINDOW');
	

	while (LOGELEMENT.hasChildNodes())
		LOGELEMENT.removeChild(LOGELEMENT.lastChild);
}

// this will set the domain to the current domain
// minus the leading host name

function setDomain()
{
	var domainNames = window.document.domain.split('.');

	if (domainNames.length > 2)
	{
		domainNames   = domainNames.slice(1);
		window.document.domain = domainNames.join('.');
	}
}
