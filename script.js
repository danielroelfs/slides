//jQuery(document).ready(function($) {
//    $('*[data-href]').on('click', function() {
//        window.location = $(this).data("href");
//    });
//});

$(document).ready(function ($) {
    $(".newtab").click(function () {
        window.open($(this).data("href"), $(this).data("target")); // Open new tab line
    });
});

$(document).ready(function ($) {
    $(".sametab").click(function () {
        window.location = $(this).data("href");
    });
});

// Show favicon on every page
window.onload = function() {
    var link = top.document.createElement("link");
    link.type = "image/png";
    link.rel = "icon";
    link.href = "https://danielroelfs.com/images/avatar.png";
    top.document.getElementsByTagName("head")[0].appendChild(link);

    var umamiscript = document.createElement("script");
    umamiscript.async = true
    umamiscript.src = "https://web-analytics.danielroelfs.app/script.js";
    umamiscript.setAttribute("data-website-id", "c5dc13e7-2365-4718-bc11-6da594f9c581");
    document.head.appendChild(umamiscript);
}