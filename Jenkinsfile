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
            parallel (['3.07', '3.08.4', '3.09.3'].collect {
                stage(it) {
                    steps {
                        sh "opam switch $it && eval `opam config env` && mkdir build/$it && cd build/$it && ../../configure && make && make tests"
                    }
                }
            })
        }
        stage('Deploy') {
            steps {
                sh 'cd build && make dist'
                archiveArtifacts artifacts: 'build/*.tar.gz', fingerprint: true
            }
        }
    }
}
