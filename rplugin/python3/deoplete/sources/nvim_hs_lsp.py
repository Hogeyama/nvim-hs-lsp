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
        self.filetypes = vim.eval(
            "get(g:, 'NvimHsLsp_serverCommands', {})").keys()

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
            # TODO レコードで渡す
            #self.vim.funcs.NvimHsLspAsyncComplete(
            #        {"bufnum": bufnum, "lnum": lnum,
            #        "col": context["complete_position"]})
        return []
