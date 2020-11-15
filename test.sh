#!/bin/bash
#
# watch - https://github.com/mikeal/watch
#
# Timeout
# https://stackoverflow.com/questions/687948/timeout-a-command-in-bash-without-unnecessary-delay
#
# Run test when a file is updated by vim.
# It has a timeout to stop an infinite loop.
watchmedo shell-command \
          --patterns="*rkt" \
          --command="if [ \"\${watch_event_type}\" == \"created\" ];then \
                        echo '------------------------'; \
                        gtimeout 3 raco test \"\${watch_src_path}\"
                    fi" \

