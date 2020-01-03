atreplinit() do repl
    try
        @eval using Pkg
        haskey(Pkg.installed(), "Revise") || @eval Pkg.add("Revise")
    catch
    end
    try
        @eval using Revise
        @async Revise.wait_steal_repl_backend()
    catch
    end
end
