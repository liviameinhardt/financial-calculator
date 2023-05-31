const themes = {
  dark: 'shinythemes/css/darkly.min.css',
  light: 'shinythemes/css/flatly.min.css'
}

// create new <link>
function newLink(theme) {
    let el = document.createElement('link');
    el.setAttribute('rel', 'stylesheet');
    el.setAttribute('type', 'text/css');
    el.setAttribute('href', theme);
    return el;
}

// remove <link> by matching the href attribute
function removeLink(theme) {
    let el = document.querySelector(`link[href='${theme}']`);
    if (el) {
        el.parentNode.removeChild(el);
    }
}

// get the <head> element
const head = document.head || document.getElementsByTagName('head')[0];

// create new <link> elements for the themes
const darkTheme = newLink(themes.dark);
const lightTheme = newLink(themes.light);

// add the light theme by default
head.appendChild(lightTheme);

// toggle switch event listener
$(document).on("change", "#themeToggle", function(event) {
    if ($(this).prop('checked')) {
        removeLink(themes.light);
        head.appendChild(darkTheme);
    } else {
        removeLink(themes.dark);
        head.appendChild(lightTheme);
    }
});