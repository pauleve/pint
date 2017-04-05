
function load_with_upload(Jupyter, ssid, input) {

    function callback(out_data) {
        var cell_element = $("#"+ssid).parents('.cell');
        var cell_idx = Jupyter.notebook.get_cell_elements().index(cell_element);
        var cell = Jupyter.notebook.get_cell(cell_idx);

        var filename = out_data.content.text;

        var code = cell.get_text();
        code = code.replace(/\bpypint.load\(\s*((format|simplify)=[^\)]*)?\)/,
                "pypint.load(\""+filename+"\",$1)").replace('",)', '")')
        cell.set_text(code);

        Jupyter.notebook.select(cell_idx);
        Jupyter.notebook.execute_cell_and_select_below();
    }

    if (! (window.File && window.FileReader && window.FileList && window.Blob)) {
        alert("Interactive file upload is not supported by your browser.");
        return;
    }

    var f = input.files[0];
    var reader = new FileReader();
    reader.onload = (function(f) {
        return function (e) {
            var obj = {
                content: e.target.result,
                name: f.name
            };
            var pycb = "pypint._js_load_callback("+JSON.stringify(obj)+")"
            IPython.notebook.kernel.execute(pycb, {iopub: {output: callback}});
        };
    })(f);
    reader.readAsDataURL(f);
}

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

    var pint_menu = {name: "Pint",
        'sub-menu': [
            {name:"Load model",
                snippet:["model = pypint.load(\"filename_or_URL\")"]},
            {name:"Upload model",
                snippet:["model = pypint.load()"]},
            "---",
            {name:"Model description",
                "sub-menu": [
                {name: "Dependency graph",
                    snippet:['model.dependency_graph()']},
                {name: "Model summary",
                    snippet:['model.summary()']}
                ]},
            {name:"Model export",
                "sub-menu": [
                {name: "Pint native format (.an)",
                    snippet:['model.export("an")']},
                {name: "NuSMV model (.smv)",
                    snippet:['model.export("nusmv")']},
                {name: "Safe Petri net in PEP format (.ll)",
                    snippet:['model.export("pep")']},
                {name: "Safe Petri net in ROMEO format (.xml)",
                    snippet:['model.export("romeo")']},
                ]},
            "---",
            {name:"Model transformation",
                "sub-menu": [
                {name: "Change initial state",
                    snippet:['model.having(a=1,b=1)']},
                {name: "Lock automata (mutations)",
                    snippet:['model.lock({"a":1,"b":0})']},
                {name: "Disable local states",
                    snippet:['model.disable({"a":1,"b":1})']},
                {name: "Goal-oriented reduction",
                    snippet:['model.reduce_for_goal("a=1")']}
                ]},
            "---",
            {name:"Compute mutations for cutting goal reachability",
                snippet:['model.oneshot_mutations_for_cut("a=1")']},
            {name:"Compute cut sets of paths to goal",
                snippet:['model.cutsets("a=1")']},
            {name:"Compute bifurcation transitions from goal",
                snippet:['model.bifurcations("a=1")']},
            {name:"Verify reachability of goal",
                snippet:['model.reachability("a=1")']},
            "---",
            {name:"Local Causality Graph",
                "sub-menu": [
                {name: "Full LCG", snippet:['model.full_lcg()']},
                {name: "Simple LCG for goal reachability over-approximation",
                    snippet:['model.simple_lcg("a=1")']},
                {name: "Saturated LCG for goal reachability under-approximation",
                    snippet:['model.saturated_lcg("a=1")']},
                {name: "Worth LCG for goal-oriented model reduction",
                    snippet:['model.worth_lcg("a=1")']}
                ]},
            "---",
            {name:"State graph analysis",
                "sub-menu": [
                {name: "Count reachable states",
                    snippet:['model.count_reachable_states()']},
                {name: "Reachable state graph",
                    snippet:['model.reachable_stategraph()']},
                {name: "Reachable attractors",
                    snippet:['model.reachable_attractors()']},
                {name: "Fixpoints",
                    snippet:['model.fixpoints()']}
                ]},
            "---",
            {name:"Goal specification",
                "sub-menu": [
                {name: "Simple goal", snippet:['"a=1"']},
                {name: "Sub-state goal", snippet:['"a=1,b=1"']},
                {name: "Sequence of simple goals", snippet:['"a=1","b=1"']},
                {name: "Sequence of sub-state goals", snippet:['"a=1,c=1","b=1,d=0"']},
                {name: "Alternative goals", snippet:['pypint.Goal("a=1")|pypint.Goal("b=1")']}
                ]}
        ]
    }

    function insert_snippet_code(snippet) {
        var cell = Jupyter.notebook.get_selected_cell();
        Jupyter.notebook.edit_mode();
        cell.code_mirror.replaceSelection(snippet, 'around');
        //cell.set_text('model = pint.load()');
        //cell.focus_editor();
    }



    /**
        from https://github.com/moble/jupyter_boilerplate/blob/master/main.js
    */
    function callback_insert_snippet (evt) {
        // this (or event.currentTarget, see below) always refers to the DOM
        // element the listener was attached to - see
        // http://stackoverflow.com/questions/12077859
        insert_snippet_code($(evt.currentTarget).data('snippet-code'));
    }
    function build_menu_element (menu_item_spec, direction) {
        // Create the menu item html element
        var element = $('<li/>');

        if (typeof menu_item_spec == 'string') {
            if (menu_item_spec != '---') {
                console.log(mod_log_prefix,
                    'Don\'t understand sub-menu string "' + menu_item_spec + '"');
                return null;
            }
            return element.addClass('divider');
        }

        var a = $('<a/>')
            .attr('href', '#')
            .html(menu_item_spec.name)
            .appendTo(element);
        if (menu_item_spec.hasOwnProperty('snippet')) {
            var snippet = menu_item_spec.snippet;
            if (typeof snippet == 'string' || snippet instanceof String) {
                snippet = [snippet];
            }
            a.attr({
                'title' : "", // Do not remove this, even though it's empty!
                'data-snippet-code' : snippet.join('\n'),
            })
            .on('click', callback_insert_snippet)
            .addClass('snippet');
        }
        else if (menu_item_spec.hasOwnProperty('internal-link')) {
            a.attr('href', menu_item_spec['internal-link']);
        }
        else if (menu_item_spec.hasOwnProperty('external-link')) {
            a.empty();
            a.attr({
                'target' : '_blank',
                'title' : 'Opens in a new window',
            });
            $('<i class="fa fa-external-link menu-icon pull-right"/>').appendTo(a);
            $('<span/>').html(menu_item_spec.name).appendTo(a);
        }

        if (menu_item_spec.hasOwnProperty('sub-menu')) {
            element
                .addClass('dropdown-submenu')
                .toggleClass('dropdown-submenu-left', direction === 'left');
            var sub_element = $('<ul class="dropdown-menu"/>')
                .toggleClass('dropdown-menu-compact', menu_item_spec.overlay === true) // For space-saving menus
                .appendTo(element);

            var new_direction = (menu_item_spec['sub-menu-direction'] === 'left') ? 'left' : 'right';
            for (var j=0; j<menu_item_spec['sub-menu'].length; ++j) {
                var sub_menu_item_spec = build_menu_element(menu_item_spec['sub-menu'][j], new_direction);
                if(sub_menu_item_spec !== null) {
                    sub_menu_item_spec.appendTo(sub_element);
                }
            }
        }

        return element;
    }

    function menu_setup (menu_item_specs, sibling, insert_before_sibling) {
        for (var i=0; i<menu_item_specs.length; ++i) {
            var menu_item_spec;
            if (insert_before_sibling) {
                menu_item_spec = menu_item_specs[i];
            } else {
                menu_item_spec = menu_item_specs[menu_item_specs.length-1-i];
            }
            var direction = (menu_item_spec['menu-direction'] == 'left') ? 'left' : 'right';
            var menu_element = build_menu_element(menu_item_spec, direction);
            // We need special properties if this item is in the navbar
            if ($(sibling).parent().is('ul.nav.navbar-nav')) {
                menu_element
                    .addClass('dropdown')
                    .removeClass('dropdown-submenu dropdown-submenu-left');
                menu_element.children('a')
                    .addClass('dropdown-toggle')
                    .attr({
                        'id': 'pint_menu',
                        'data-toggle' : 'dropdown',
                        'aria-expanded' : 'false'
                    });
            }

            // Insert the menu element into DOM
            menu_element[insert_before_sibling ? 'insertBefore': 'insertAfter'](sibling);
        }
    }
    /** end from */


    function self_cleanup() {
        var cell_element = $("script[class='to-be-removed']").parents('.cell');
        var cell_idx = Jupyter.notebook.get_cell_elements().index(cell_element);
        var cell = Jupyter.notebook.get_cell(cell_idx);
        var to_remove = -1;
        for (var i = 0; i < cell.output_area.outputs.length; ++i) {
            var oa = cell.output_area.outputs[i];
            if (oa.output_type == "display_data"
                && typeof oa.data["text/html"] != 'undefined'
                && oa.data["text/html"].indexOf(' class="to-be-removed"') >= 0) {
                to_remove = i;
                break;
            }
        }
        if (to_remove == -1) {
            console.log("cannot find toberemoved");
        } else {
            cell.output_area.outputs.splice(to_remove, 1);
        }
    }

    function load_ipython_extension() {
        $("#pint-toolbar").remove();
        $("#pint_menu").parent().remove();
        var prefix = "pint";

        var act_upload = Jupyter.actions.register({
            icon    : "fa-upload",
            help    : "Upload model",
            handler : function() {
                var cell = Jupyter.notebook.get_selected_cell();
                cell.set_text('model = pypint.load()');
                cell.focus_editor();
            }
        }, "upload", prefix);
        var act_enable_debug = Jupyter.actions.register({
            help    : 'Enable debug',
            handler : function() {
                IPython.notebook.kernel.execute("pypint.enable_dbg()");
                pint_ui_debug_enabled(true);
            }
        }, "enable-debug", prefix);
        var act_disable_debug = Jupyter.actions.register({
            help    : 'Disable debug',
            handler : function() {
                IPython.notebook.kernel.execute("pypint.disable_dbg()");
                pint_ui_debug_enabled(false);
            }
        }, "disable-debug", prefix);
        Jupyter.toolbar.add_buttons_group([
            act_upload,
            act_enable_debug, act_disable_debug
        ], "pint-toolbar");

        btn_enable_debug = $("#pint-toolbar > button[data-jupyter-action='pint:enable-debug']");
        btn_disable_debug = $("#pint-toolbar > button[data-jupyter-action='pint:disable-debug']");
        btn_enable_debug[0].innerHTML = "enable debug";
        btn_disable_debug[0].innerHTML = "disable debug";

        menu_setup([pint_menu], $("#help_menu").parent(), true);

        setTimeout(self_cleanup, 5000);
    };

    load_ipython_extension();
}

pint_extension(Jupyter);

