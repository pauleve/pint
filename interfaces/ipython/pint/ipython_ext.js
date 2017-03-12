
var btn_enable_debug;
var btn_disable_debug;

function pint_ui_debug_enabled(enabled) {
    if (enabled) {
        btn_enable_debug.hide();
        btn_disable_debug.show();
    } else {
        btn_enable_debug.show();
        btn_disable_debug.hide();
    }
}

function pint_extension(Jupyter) {

    function load_ipython_extension() {
        $("#pint-toolbar").remove();
        var prefix = "pint";

        var act_enable_debug = Jupyter.actions.register({
            help    : 'Enable debug',
            handler : function() {
                IPython.notebook.kernel.execute("pint.enable_dbg()");
                pint_ui_debug_enabled(true);
            }
        }, "enable-debug", prefix);
        var act_disable_debug = Jupyter.actions.register({
            help    : 'Disable debug',
            handler : function() {
                IPython.notebook.kernel.execute("pint.disable_dbg()");
                pint_ui_debug_enabled(false);
            }
        }, "disable-debug", prefix);
        Jupyter.toolbar.add_buttons_group([
            act_enable_debug, act_disable_debug
        ], "pint-toolbar");

        btn_enable_debug = $("#pint-toolbar > button[data-jupyter-action='pint:enable-debug']");
        btn_disable_debug = $("#pint-toolbar > button[data-jupyter-action='pint:disable-debug']");
        btn_enable_debug[0].innerHTML = "enable debug";
        btn_disable_debug[0].innerHTML = "disable debug";
    };

    load_ipython_extension();
}

pint_extension(Jupyter);

