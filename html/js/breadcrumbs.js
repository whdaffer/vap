// handle section bread crumbs
// works in conjunction with global templates
// uses global var set in each page to determine the breadcrumb

function setSectionBreadCrumb()
{
	if (!navigator.DOMCORE1 || !navigator.DOMHTML || !navigator.DOMCSS2)
		return;

	if (typeof(BREADCRUMB_URL) == 'undefined' || typeof(BREADCRUMB_TEXT) == 'undefined')
		return;
	
	var breadcrumbs;
	var breadcrumb;
	var link;

	breadcrumbs = document.getElementById('breadcrumbs');
	if (breadcrumbs)
	{
		breadcrumb	= breadcrumbs.appendChild(document.createElement('span'));
		link		= breadcrumb.appendChild(document.createElement('a'));
		link.className = 'topnav';
		link.href	= BREADCRUMB_URL;
		link.target	= '_top';
		link.appendChild(document.createTextNode(BREADCRUMB_TEXT));
		breadcrumb.appendChild(document.createTextNode(' > '));
	}
}

// handle case where an onload handler is already defined.
if (typeof(window.onload) != 'function')
	window.onload = setSectionBreadCrumb;
else
{
	window.oldOnLoad = window.onload;
	window.onload = new Function("evt", "setSectionBreadCrumb(); window.oldOnLoad(evt);");
}
