from . import handlers


def setup_routes(app):
    add_get = app.router.add_get
    add_post = app.router.add_post

    add_get('/protocol_version', handlers.protocol_version)
    add_get('/root', handlers.root)
    add_get('/all_resources', handlers.all_resources)
    add_get('/task/{task_id}', handlers.task_status)
    add_get('/tasks', handlers.tasks)
    add_post('/history/undo', handlers.undo)
    add_post('/history/redo', handlers.redo)
    add_get('/history/view_undo', handlers.undo_history)
    add_get('/history/view_redo', handlers.redo_history)
    add_get('/history/undo_info/{idx}', handlers.undo_info)
    add_get('/history/redo_info/{idx}', handlers.redo_info)
    add_post('/test/long_running', handlers.long_running_test)
    add_post('/refactor/rename', handlers.rename)
    add_post('/refactor/extract_method', handlers.extract_method)
    add_post('/refactor/extract_variable', handlers.extract_variable)
    add_post('/refactor/normalize_arguments', handlers.normalize_arguments)
    add_post('/refactor/remove_argument', handlers.remove_argument)
    add_post('/refactor/add_argument', handlers.add_argument)
    add_post('/code_assist/completions', handlers.code_assist_completion)
    add_post('/code_assist/doc', handlers.code_assist_doc)
    add_post('/code_assist/calltip', handlers.code_assist_calltip)
    add_post('/findit/occurrences', handlers.findit_occurences)
    add_post('/findit/implementations', handlers.findit_implementations)
    add_post('/findit/definition', handlers.findit_definitions)
    add_post("/imports/organize", handlers.organize_imports)
    add_post("/imports/expand_star", handlers.expand_star_imports)
    add_post("/imports/froms_to_imports", handlers.from_to_imports)
    add_post("/imports/relatives_to_absolutes", handlers.relatives_to_absolutes)
    add_post("/imports/handle_long_imports", handlers.handle_long_imports)
