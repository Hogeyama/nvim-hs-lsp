from .base import Base
import re

CompleteResults = "g:NvimHsLspCompleteResult"

class Source(Base):
    def __init__(self, vim):
        super().__init__(vim)

        self.rank = 100
        self.name = "nvim-hs-lsp"
        self.mark = "[nhl]"
        self.sorters = ["sorter_rank"]
        self.max_menu_width = 1000
        self.filetypes = vim.eval(
            "get(g:, 'NvimHsLsp_serverCommands', {})").keys()
        self.input_pattern += r'(\.|::|->)\w*$|\w{2,}$'

    def get_complete_position(self, context):
        m = re.search(r"\w*$", context['input'])
        return m.start() if m else -1

    def gather_candidates(self, context):
        if context["is_async"]:
            results = self.vim.eval(CompleteResults)
            if results != -1:
                context["is_async"] = False
                return results
        else:
            context["is_async"] = True
            self.vim.command("let {0} = -1".format(CompleteResults))
            [_,lnum,col,_] = self.vim.eval("getpos('.')")
            self.vim.funcs.NvimHsLspAsyncComplete(lnum,col)
        return []
