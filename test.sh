#!/bin/bash
#
# watch - https://github.com/mikeal/watch
#

# not used to test only changed
#filename="${1:-*.test.rkt}"

watchmedo shell-command \
          --patterns="*rkt" \
          --command="if [ \"\${watch_event_type}\" == \"created\" ];then \
                        echo '------------------------'; \
                        raco test \"\${watch_src_path}\";
                    fi" \

