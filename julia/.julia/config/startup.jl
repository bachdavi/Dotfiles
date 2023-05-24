try
    using Revise
    using OhMyREPL
catch e
    @warn "Error initializing Revise" exception=(e, catch_backtrace())
end
