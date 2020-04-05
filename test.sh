#!/bin/bash
#
# watch - https://github.com/mikeal/watch
#

filename="${1:-*.test.rkt}"

watchmedo shell-command \
          --patterns="*rkt" \
          --command="if [ \"\${watch_event_type}\" == \"created\" ];then \
                        echo '------------------------'; \
                        raco test $filename; \
                    fi" \

