#!/bin/bash
#
# watch - https://github.com/mikeal/watch
#

# not used to test only changed
#filename="${1:-*.test.rkt}"

# Timeout
# https://stackoverflow.com/questions/687948/timeout-a-command-in-bash-without-unnecessary-delay

watchmedo shell-command \
          --patterns="*rkt" \
          --command="if [ \"\${watch_event_type}\" == \"created\" ];then \
                        echo '------------------------'; \
                        gtimeout 3 raco test \"\${watch_src_path}\"
                    fi" \

