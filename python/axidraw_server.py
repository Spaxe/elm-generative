#!/usr/bin/env python3
"""
A simple webapp that works with the elm-generative webpage
EXTREMELY EXPERIMENTAL
Author: Xavier Ho <xavier@jtg.design>

## To start axidraw-server on port 6743, listening from all hosts

    python axidraw_server.py

## REST API usage:

  * Content-Type is always `application/json`
  * Success is 200 OK (or any of the 2xx family)
  * Failure is 4xx or 5xx
    - Failure response structure:

        {
          "reason": "..."
        }
"""

# For as long as axidraw-standalone is in private beta, we need to copy the
# module side by side with this module, and import it here
import os
import sys
import subprocess

import json
import time
import tempfile
import traceback
from flask import Flask
from flask_restful import Resource, Api, reqparse, abort
from flask_cors import CORS

app = Flask(__name__)
api = Api(app)
cors = CORS(app, resources={r"/*": {"origins": "*"}})


class Print(Resource):
    """
    Manages plotter operations.
    """

    def get(self):
        """### GET /print

        Inquires the current plotter version
        """
        result = subprocess.run(["axicli", "--mode", "manual"], capture_output=True)
        return json.dumps({"version": str(result.stderr)})

    def post(self):
        """### POST /print

        Sends a SVG document to the printer, and immediately starts printing.

        Response body: (see GET /print)
        """
        try:
            parser = reqparse.RequestParser()
            parser.add_argument("svg")
            args = parser.parse_args()
            data = args["svg"]

            subprocess.run(["axicli", "--mode", "manual", "-M", "enable_xy"])
            subprocess.run(["axicli", "--mode", "manual", "-M", "raise_pen"])
            with tempfile.NamedTemporaryFile(dir="C:\\crap", delete=False) as fp:
                fp.write(data.encode())
                fp.seek(0)
                print(fp.name)
                time.sleep(1)
                subprocess.run(["axicli", fp.name])

            return self.get()

        except:
            abort(500, reason=traceback.format_exc())

    def put(self):
        """### PUT /print

        Toggle between raising or lowering the pen

        Request body: (empty)

        Response body: (see GET /print)
        """
        subprocess.run(["axicli", "--mode", "manual", "-M", "enable_xy"])
        subprocess.run(["axicli", "--mode", "toggle"])

        return self.get()

    def delete(self):
        """### DELETE /print

        Turns the motor off.

        Request body: (empty)

        Response body: (see GET /print)
        """
        subprocess.run(["axicli", "--mode", "manual", "-M", "enable_xy"])
        subprocess.run(["axicli", "--mode", "manual", "-M", "raise_pen"])
        subprocess.run(["axicli", "--mode", "manual", "-M", "disable_xy"])

        return self.get()


api.add_resource(Print, "/print")


if __name__ == "__main__":
    app.run(debug=True, port=6743, host="0.0.0.0")
