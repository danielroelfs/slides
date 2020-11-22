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