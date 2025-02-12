from flask import Flask, request, send_from_directory, jsonify
import subprocess
import os

app = Flask(__name__, static_folder='html_build', static_url_path='')

PROJECT_ROOT = os.path.abspath(os.path.join(os.path.dirname(__file__), ".."))

@app.route('/')
def index():
    return send_from_directory(app.static_folder, 'index.html')

@app.route("/get-includes-path", methods=["GET"])
def get_includes_path():
    includes_path = os.path.join(PROJECT_ROOT, "testdata/arch")
    return jsonify({"path": includes_path})


@app.route("/preprocess", methods=["GET"])
def preprocess():
    filename = request.args.get("filename")
    includes = request.args.getlist("includes")

    if not filename:
        return jsonify({"error": "No filename provided"}), 400

    cmd = ["cc"] + [f"-I{inc}" for inc in includes] + ["-undef", "-nostdinc", "-E", "-x", "c", filename]

    try:
        result = subprocess.run(cmd, stdout=subprocess.PIPE, stderr=subprocess.PIPE, text=True, check=True)
        return jsonify({"output": result.stdout})
    except subprocess.CalledProcessError as e:
        return jsonify({"error": e.stderr}), 500

@app.route("/stf", methods=["GET"])
def read_stf():
    filename = request.args.get("filename")
    if not filename:
        return jsonify({"error": "No filename provided"}), 400

    try:
        with open(filename, "r", encoding="utf-8") as f:
            content = f.read()
        return jsonify({"output": content})
    except Exception as e:
        return jsonify({"error": str(e)}), 500

@app.route("/save_files", methods=["POST"])
def save_files():
    data = request.get_json()
    p4_code = data.get("p4_code")
    packet = data.get("packet")
    if not p4_code or not packet:
        return jsonify({"error": "Missing p4_code or packet"}), 400

    p4_filename = f"/tmp/program.p4"
    stf_filename = f"/tmp/program.stf"

    try:
        with open(p4_filename, "w", encoding="utf-8") as f:
            f.write(p4_code)
        with open(stf_filename, "w", encoding="utf-8") as f:
            f.write(packet)
        return jsonify({"filename": p4_filename, "stfname": stf_filename})
    except Exception as e:
        return jsonify({"error": str(e)}), 500

@app.route('/favicon.ico', methods=['GET'])
def favicon():
    return '', 204

if __name__ == "__main__":
    app.run(host="0.0.0.0", port=8080)
