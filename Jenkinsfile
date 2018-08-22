/*
def branches = ['3.07', '3.08.4', '3.09.3'].collect {
    stage(it) {
        steps {
            sh "opam switch $it && eval `opam config env` && mkdir build/$it && cd build/$it && ../../configure && make && make tests"
        }
    }
}
*/
def branches = {
    stage('3.07') {
        steps {
            sh "opam switch 3.07 && eval `opam config env` && mkdir build/3.07 && cd build/3.07 && ../../configure && make && make tests"
        }
    }
}

pipeline {
    agent {
        dockerfile {
            label 'slave'
        }
    }

    stages {
        stage('Build') {
            steps {
                sh 'eval `opam config env` && autoreconf && mkdir build && cd build && ../configure && make'
            }
        }
        stage('Test') {
            parallel branches
        }
        stage('Deploy') {
            steps {
                sh 'cd build && make dist'
                archiveArtifacts artifacts: 'build/*.tar.gz', fingerprint: true
            }
        }
    }
}
